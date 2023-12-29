module Day20.InputParser where
    import Data.List.Split
    import Day20.Machine
    import Util

    parseInput input = findInputsForConjunctionModules Machine {
        modules = modulesWithoutBroadcaster,
        broadcasterDestinations = (destinations broadcaster),
        highPulsesSent = 0,
        lowPulsesSent = 0
    } where
        modulesWithoutBroadcaster = filter (\m -> name m /= "broadcaster") modules
        broadcaster = head (filter (\m -> name m == "broadcaster") modules)
        modules = map (parseLine.trim) (lines input)

    parseLine :: String -> Module
    parseLine ('b':'r':'o':'a':'d':'c':'a':'s':'t':'e':'r':xs) = parseFlipFlop ("broadcaster" ++ xs)
    parseLine ('%':xs) = parseFlipFlop xs
    parseLine ('&':xs) = parseConjunction xs

    parseFlipFlop info = FlipFlop {
        name = head moduleParts,
        state = False,
        destinations = destinations
    } where
        moduleParts = splitOn " -> " info
        destinations = map trim (splitOn "," (last moduleParts))

    parseConjunction info = Conjunction {
        name = head moduleParts,
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
        lowPulsesSent = lowPulsesSent machineState
    }

    findInputsForConjunctionModule :: Machine -> Module -> Module
    findInputsForConjunctionModule machineState moduleState = (case moduleState of
        Conjunction name states destinations -> Conjunction {
            name = name,
            states = map (\i -> SignalState { ssName = i, ssState = LowPulse }) inputs,
            destinations = destinations
        }
        _           -> moduleState) where
        inputs = map name (filter (\m -> elem moduleName (destinations m)) (modules machineState))
        moduleName = name moduleState
