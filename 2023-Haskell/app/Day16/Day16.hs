module Day16.Day16 where
    import Grid
    import Util

    part1 :: String -> String
    part2 :: String -> String

    data BeamDirection = N | S | E | W deriving (Show, Eq)
    data Cell = Cell { xPos :: Int, yPos :: Int} deriving (Show, Eq)
    data Beam = Beam { direction :: BeamDirection, cell :: Cell } deriving (Show, Eq)
    data EnergiseState = EnergiseState { energisedCells :: [Cell], beams :: [Beam], historicBeams :: [Beam] } deriving (Show)

    part1 input = show $ findNumberOfEnergisedTiles (convertToGrid input) Beam { direction = E, cell = Cell { xPos = 0, yPos = 0}}

    part2 input = show $ maximum (map (\b -> findNumberOfEnergisedTiles grid b) (getPossibleInputBeams grid)) where
         grid = convertToGrid input

    getPossibleInputBeams :: Grid Char -> [Beam]
    getPossibleInputBeams grid =
        map (\i -> Beam { direction = E, cell = Cell { xPos = 0, yPos = i}}) (range 0 ((height grid) - 1))
        ++ map (\i -> Beam { direction = S, cell = Cell { xPos = i, yPos = 0}}) (range 0 ((width grid) - 1))
        ++ map (\i -> Beam { direction = W, cell = Cell { xPos = ((width grid) - 1), yPos = i}}) (range 0 ((height grid) - 1))
        ++ map (\i -> Beam { direction = N, cell = Cell { xPos = i, yPos = ((height grid) - 1)}}) (range 0 ((width grid) - 1))

    findNumberOfEnergisedTiles grid startBeam = length (rmdups (energisedCells (head (dropWhile (\s -> length (beams s) > 0) statesList)))) where
        statesList = iterate (\s -> progressState s grid) state
        state = initialState startBeam

    initialState :: Beam -> EnergiseState
    initialState startBeam = EnergiseState {
        energisedCells = [(cell startBeam)],
        beams = [startBeam],
        historicBeams = [startBeam]
    }

    progressState :: EnergiseState -> Grid Char -> EnergiseState
    progressState state grid = EnergiseState {
        energisedCells = addToEnergisedCells (energisedCells state) newBeams,
        beams = newBeams,
        historicBeams = (historicBeams state) ++ newBeams
    } where
        newBeams = removeAlreadySeenBeams (historicBeams state) (removeOffGridBeams (map (\b -> progressBeam b grid) (bounceBeams (beams state) grid)) grid)

    bounceBeams :: [Beam] -> Grid Char -> [Beam]
    bounceBeams beams grid = concat (map (\b -> bounceBeam b grid) beams)

    bounceBeam :: Beam -> Grid Char -> [Beam]
    bounceBeam beam grid = if and [getCharAtBeam beam grid == '-', or [direction beam == N, direction beam == S]]
            then [Beam { direction = W, cell = cell beam }, Beam { direction = E, cell = cell beam }]
        else if and [getCharAtBeam beam grid == '|', or [direction beam == W, direction beam == E]]
            then [Beam { direction = N, cell = cell beam }, Beam { direction = S, cell = cell beam }]
        else if getCharAtBeam beam grid == '/'
            then if direction beam == N
                then [Beam { direction = E, cell = cell beam }]
            else if direction beam == S
                then [Beam { direction = W, cell = cell beam }]
            else if direction beam == E
                then [Beam { direction = N, cell = cell beam }]
            else [Beam { direction = S, cell = cell beam }]
        else if getCharAtBeam beam grid == '\\'
            then if direction beam == N
                then [Beam { direction = W, cell = cell beam }]
            else if direction beam == S
                then [Beam { direction = E, cell = cell beam }]
            else if direction beam == W
                then [Beam { direction = N, cell = cell beam }]
            else [Beam { direction = S, cell = cell beam }]
        else [beam]

    getCharAtBeam :: Beam -> Grid Char -> Char
    getCharAtBeam beam grid = getGridCell grid (yPos (cell beam), xPos (cell beam))

    progressBeam :: Beam -> Grid Char -> Beam
    progressBeam beam grid = (case (direction beam) of
        N -> Beam { direction = N, cell = Cell { xPos = x, yPos = y - 1}}
        S -> Beam { direction = S, cell = Cell { xPos = x, yPos = y + 1}}
        E -> Beam { direction = E, cell = Cell { xPos = x + 1, yPos = y}}
        W -> Beam { direction = W, cell = Cell { xPos = x - 1, yPos = y}}) where
        x = xPos (cell beam)
        y = yPos (cell beam)

    addToEnergisedCells :: [Cell] -> [Beam] -> [Cell]
    addToEnergisedCells cells beams = concat [cells, map cell beams]

    removeOffGridBeams :: [Beam] -> Grid Char -> [Beam]
    removeOffGridBeams beams grid = filter (\b -> beamIsOnGrid b grid) beams

    beamIsOnGrid :: Beam -> Grid Char -> Bool
    beamIsOnGrid beam grid = and[x >= 0, x < width grid, y >= 0, y < height grid] where
        x = (xPos (cell beam))
        y = (yPos (cell beam))

    removeAlreadySeenBeams :: [Beam] -> [Beam] -> [Beam]
    removeAlreadySeenBeams historicBeams beams = filter (\b -> not (elem b historicBeams)) beams
