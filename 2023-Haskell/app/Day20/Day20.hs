module Day20.Day20 where
    import Debug.Trace
    import Day20.InputParser (parseInput)
    import Day20.Machine

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ (highPulsesSent finalMachineState) * (lowPulsesSent finalMachineState) where
        finalMachineState = pressButtonTimes (parseInput input) 1000

    part2 input = show $ foldl lcm 1 (map (\bp -> buttonPresses bp) (buttonPressData (countButtonPressesUntilRxIsSentLowPulse (parseInput input) 0)))

    pressButtonTimes :: Machine -> Int -> Machine
    pressButtonTimes machine pressCount = head (drop pressCount (iterate (\x -> pressButton x 0) machine))

    countButtonPressesUntilRxIsSentLowPulse :: Machine -> Int -> Machine
    countButtonPressesUntilRxIsSentLowPulse machine pressCount =
        if and[length (filter (\bp -> buttonPresses bp == 0) (buttonPressData machine)) == 0, length (buttonPressData machine) > 0]
            then machine
            else countButtonPressesUntilRxIsSentLowPulse (pressButton machine (pressCount + 1)) (pressCount + 1)

    pressButton :: Machine -> Int -> Machine
    pressButton machine pressCount = sendPulseToModule
        (addPulse machine LowPulse)
        (map (\m -> PulseTransmission { moduleTo = m, moduleFrom = "broadcaster", pulseType = LowPulse }) (broadcasterDestinations machine))
        pressCount

    sendPulseToModule :: Machine -> [PulseTransmission] -> Int -> Machine
    sendPulseToModule machine pulseTransmissionQueue pressCount = if length pulseTransmissionQueue == 0
        then machine
        else if not (moduleExists machine moduleName)
            then sendPulseToModule newMachine (drop 1 pulseTransmissionQueue) pressCount
            else if moduleType == ConjunctionModule
                then sendPulseToConjunction newMachine pulseTransmissionQueue pressCount
                else sendPulseToFlipFlop newMachine pulseTransmissionQueue pressCount where
                    thisPulseTransmission = head pulseTransmissionQueue
                    moduleName = moduleTo thisPulseTransmission
                    fromModuleName = moduleFrom thisPulseTransmission
                    thisPulseType = pulseType thisPulseTransmission
                    moduleType = mType (getModule newMachine moduleName)
                    newMachine = if elem fromModuleName (map (\bp -> input bp) (buttonPressData machine)) then pulseSentToRxInput machineWithPulse thisPulseType fromModuleName pressCount else machineWithPulse
                    machineWithPulse = (addPulse machine thisPulseType)

    pulseSentToRxInput :: Machine -> PulseType -> String -> Int -> Machine
    pulseSentToRxInput machine thisPulseType moduleName pressCount = if thisPulseType == LowPulse
        then machine
        else Machine {
            modules = modules machine,
            broadcasterDestinations = broadcasterDestinations machine,
            highPulsesSent = highPulsesSent machine,
            lowPulsesSent = lowPulsesSent machine,
            buttonPressData = map (\bp -> if and[input bp == moduleName, buttonPresses bp == 0] then newBPData else bp) (buttonPressData machine)
        } where
            newBPData = ButtonPressData { input = moduleName, buttonPresses = pressCount }

    sendPulseToFlipFlop :: Machine -> [PulseTransmission] -> Int -> Machine
    sendPulseToFlipFlop machine pulseTransmissionQueue pressCount = if thisPulseType == HighPulse
        then sendPulseToModule machine (drop 1 pulseTransmissionQueue) pressCount
        else sendPulseToModule newMachine ((drop 1 pulseTransmissionQueue) ++ pulsesToSend) pressCount where
           pulsesToSend = map (\m -> PulseTransmission { moduleTo = m, moduleFrom = moduleName, pulseType = pulseTypeToSend }) (destinations (getModule newMachine moduleName))
           moduleName = moduleTo thisPulseTransmission
           thisPulseType = pulseType thisPulseTransmission
           fromModuleName = moduleFrom thisPulseTransmission
           thisPulseTransmission = head pulseTransmissionQueue
           newMachine = flipFlipFlop machine moduleName
           pulseTypeToSend = if state (getModule newMachine moduleName) then HighPulse else LowPulse

    sendPulseToConjunction :: Machine -> [PulseTransmission] -> Int -> Machine
    sendPulseToConjunction machine pulseTransmissionQueue pressCount = sendPulseToModule newMachine ((drop 1 pulseTransmissionQueue) ++ pulsesToSend) pressCount where
        moduleName = (moduleTo thisPulseTransmission)
        thisPulseType = (pulseType thisPulseTransmission)
        fromModuleName = (moduleFrom thisPulseTransmission)
        thisPulseTransmission = head pulseTransmissionQueue
        pulsesToSend = map (\m -> PulseTransmission { moduleTo = m, moduleFrom = moduleName, pulseType = pulseTypeToSend }) (destinations (getModule newMachine moduleName))
        newMachine = Machine {
            modules = map (\m -> if name m == moduleName then newModule else m) (modules machine),
            broadcasterDestinations = broadcasterDestinations machine,
            highPulsesSent = highPulsesSent machine,
            lowPulsesSent = lowPulsesSent machine,
            buttonPressData = buttonPressData machine
        }
        currentModule = getModule machine moduleName
        newModule = Conjunction {
            name = moduleName,
            mType = ConjunctionModule,
            states = map (\s -> if ssName s == fromModuleName then newSignalState else s) (states currentModule),
            destinations = destinations currentModule
        }
        newSignalState = SignalState {
            ssName = fromModuleName,
            ssState = thisPulseType
        }
        pulseTypeToSend = if length (filter (\s -> ssState s == HighPulse) (states newModule)) == length (states newModule) then LowPulse else HighPulse

    addPulse :: Machine -> PulseType -> Machine
    addPulse machine pulse = if pulse == HighPulse
        then Machine {
            modules = modules machine,
            broadcasterDestinations = broadcasterDestinations machine,
            highPulsesSent = (highPulsesSent machine) + 1,
            lowPulsesSent = lowPulsesSent machine,
            buttonPressData = buttonPressData machine
        } else Machine {
            modules = modules machine,
            broadcasterDestinations = broadcasterDestinations machine,
            highPulsesSent = highPulsesSent machine,
            lowPulsesSent = (lowPulsesSent machine) + 1,
            buttonPressData = buttonPressData machine
        }

    moduleExists :: Machine -> String -> Bool
    moduleExists machine moduleName = length (filter (\m -> name m == moduleName) (modules machine)) == 1

    getModule :: Machine -> String -> Module
    getModule machine moduleName = head (filter (\m -> name m == moduleName) (modules machine))

    flipFlipFlop :: Machine -> String -> Machine
    flipFlipFlop machine flipFlopName = newMachine where
        newMachine = Machine {
            modules = map (\m -> if name m == flipFlopName then flipFlipFlopModule m else m) (modules machine),
            broadcasterDestinations = broadcasterDestinations machine,
            lowPulsesSent = lowPulsesSent machine,
            highPulsesSent = highPulsesSent machine,
            buttonPressData = buttonPressData machine
        }

    flipFlipFlopModule :: Module -> Module
    flipFlipFlopModule thisModule = FlipFlop {
        name = name thisModule,
        mType = FlipFlopModule,
        state = not (state thisModule),
        destinations = destinations thisModule
    }
