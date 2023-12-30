module Day20.Machine where
    data Machine = Machine {
        modules :: [Module],
        broadcasterDestinations :: [String],
        highPulsesSent :: Int,
        lowPulsesSent :: Int,
        buttonPressData :: [ButtonPressData] } deriving (Show)

    data ModuleType = FlipFlopModule | ConjunctionModule deriving (Eq, Show)

    data Module = FlipFlop { name :: String, mType :: ModuleType, state :: Bool, destinations :: [String] }
                | Conjunction { name :: String, mType :: ModuleType, states :: [SignalState], destinations :: [String] } deriving (Show, Eq)

    data SignalState = SignalState { ssName :: String, ssState :: PulseType } deriving (Show, Eq)

    data ButtonPressData = ButtonPressData { input :: String, buttonPresses :: Int} deriving (Show)

    data PulseType = HighPulse | LowPulse deriving (Show, Eq)

    data PulseTransmission = PulseTransmission { moduleFrom :: String, moduleTo :: String, pulseType :: PulseType } deriving (Show, Eq)
