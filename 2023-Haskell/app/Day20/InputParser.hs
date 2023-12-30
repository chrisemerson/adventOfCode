module Day20.InputParser where
    import Data.List.Split
    import Debug.Trace
    import Day20.Machine
    import Util

    parseInput input = findInputsForRxConjunction (findInputsForConjunctionModules Machine {
        modules = modulesWithoutBroadcaster,
        broadcasterDestinations = (destinations broadcaster),
        highPulsesSent = 0,
        lowPulsesSent = 0,
        buttonPressData = []
    }) where
        modulesWithoutBroadcaster = filter (\m -> name m /= "broadcaster") modules
        broadcaster = head (filter (\m -> name m == "broadcaster") modules)
        modules = map (parseLine.trim) (lines input)

    parseLine :: String -> Module
    parseLine ('b':'r':'o':'a':'d':'c':'a':'s':'t':'e':'r':xs) = parseFlipFlop ("broadcaster" ++ xs)
    parseLine ('%':xs) = parseFlipFlop xs
    parseLine ('&':xs) = parseConjunction xs

    parseFlipFlop info = FlipFlop {
        name = head moduleParts,
        mType = FlipFlopModule,
        state = False,
        destinations = destinations
    } where
        moduleParts = splitOn " -> " info
        destinations = map trim (splitOn "," (last moduleParts))

    parseConjunction info = Conjunction {
        name = head moduleParts,
        mType = ConjunctionModule,
        states = map (\d -> SignalState { ssName = d, ssState = LowPulse }) inputs,
        destinations = destinations
    } where
        moduleParts = splitOn " -> " info
        destinations = map trim (splitOn "," (last moduleParts))
        inputs = []

    findInputsForConjunctionModules :: Machine -> Machine
    findInputsForConjunctionModules machineState = Machine {
        modules = map (findInputsForConjunctionModule machineState) (modules machineState),
        broadcasterDestinations = broadcasterDestinations machineState,
        highPulsesSent = highPulsesSent machineState,
        lowPulsesSent = lowPulsesSent machineState,
        buttonPressData = buttonPressData machineState
    }

    findInputsForRxConjunction :: Machine -> Machine
    findInputsForRxConjunction machineState = Machine {
        modules = modules machineState,
        broadcasterDestinations = broadcasterDestinations machineState,
        highPulsesSent = highPulsesSent machineState,
        lowPulsesSent = lowPulsesSent machineState,
        buttonPressData = map (\i -> ButtonPressData { input = i, buttonPresses = 0 }) rxInputInputs
    } where
        rxInputInputs = if length rxModules == 0 then [] else map (\s -> ssName s) (states (head rxModules))
        rxModules = (filter (\m -> elem "rx" (destinations m)) (modules machineState))

    findInputsForConjunctionModule :: Machine -> Module -> Module
    findInputsForConjunctionModule machineState moduleState = if mType moduleState == ConjunctionModule
        then Conjunction {
            name = (name moduleState),
            mType = ConjunctionModule,
            states = map (\i -> SignalState { ssName = i, ssState = LowPulse }) inputs,
            destinations = (destinations moduleState)
        }
        else moduleState where
            inputs = map name (filter (\m -> elem moduleName (destinations m)) (modules machineState))
            moduleName = name moduleState
