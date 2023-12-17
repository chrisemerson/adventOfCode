module Day16.Day16 where
    import Grid

    part1 :: String -> String
    part2 :: String -> String

    data BeamDirection = N | S | E | W deriving (Show)
    data Cell = Cell { xPos :: Int, yPos :: Int} deriving (Show)
    data Beam = Beam { direction :: BeamDirection, cell :: Cell } deriving (Show)
    data EnergiseState = EnergiseState { energisedCells :: [Cell], beams :: [Beam] } deriving (Show)

    part1 input = show $ take 10 (iterate (\s -> progressState s grid) initialState) where
        grid = convertToGrid input
        state = initialState

    part2 input = input

    initialState = EnergiseState { energisedCells = [Cell { xPos = 0, yPos = 0 }], beams = [Beam { direction = E, cell = Cell { xPos = 0, yPos = 0 }}]}

    progressState :: EnergiseState -> Grid -> EnergiseState
    progressState state grid = state

    progressBeam :: Beam -> Grid -> Beam
    progressBeam beam grid = beam
