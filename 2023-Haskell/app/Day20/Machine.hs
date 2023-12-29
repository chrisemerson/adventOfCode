module Day20.Machine where
    data PulseType = HighPulse | LowPulse deriving (Show, Eq)
    data Modules = Modules { modules :: [Module], broadcasterDestinations :: [String], highPulsesSent :: Int, lowPulsesSent :: Int } deriving (Show)
    data SignalState = SignalState { ssName :: String, ssState :: PulseType } deriving (Show)
    data Module = FlipFlop { name :: String, state :: Bool, destinations :: [String] }
                | Conjunction { name :: String, states :: [SignalState], destinations :: [String] } deriving (Show)
