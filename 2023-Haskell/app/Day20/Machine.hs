module Day20.Machine where
    data Machine = Machine {
        modules :: [Module],
        broadcasterDestinations :: [String],
        highPulsesSent :: Int,
        lowPulsesSent :: Int } deriving (Show)

    data Module = FlipFlop { name :: String, state :: Bool, destinations :: [String] }
                | Conjunction { name :: String, states :: [SignalState], destinations :: [String] } deriving (Show)

    data SignalState = SignalState { ssName :: String, ssState :: PulseType } deriving (Show)

    data PulseType = HighPulse | LowPulse deriving (Show, Eq)
