module Day10.Day10 where
    import Data.Vector (fromList, head, Vector, ifoldl, ifilter, imap, map, empty, (//))
    import Grid
    import Util

    vmap = Data.Vector.map
    pmap = Prelude.map

    vhead = Data.Vector.head
    phead = Prelude.head

    data Direction = N | S | E | W deriving (Enum, Show, Eq)
    data Scout = Scout { pos :: (Int, Int), dir :: Direction } deriving (Show)
    data ScoutState = ScoutState { scout1 :: Scout, scout2 :: Scout, dist :: Int } deriving (Show)

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ findFurthestPipeDistance grid where
        grid = convertToGrid input

    part2 input = show $ findNoCellsInsidePipes gridWithoutStart pipeLocations where
        pipeLocations = getPipeLocations grid
        grid = convertToGrid input
        gridWithoutStart = getGridWithoutStart grid

    getInitialScoutState grid = ScoutState {
            scout1 = Scout { pos = startPos, dir = fst directions},
            scout2 = Scout { pos = startPos, dir = snd directions},
            dist = 0
        } where
        directions = findDirections grid startPos
        startPos = findStart grid

    findFurthestPipeDistance grid = dist (phead (dropWhile (not.scoutsInSamePlace) (iterateScoutPaths grid)))

    iterateScoutPaths grid = iterate (\x -> advanceScouts x grid) (advanceScouts (getInitialScoutState grid) grid)

    getPipeLocations grid =
        [findStart grid]
        ++ concat (pmap (\x -> [pos (scout1 x), pos (scout2 x)]) (take (findFurthestPipeDistance grid) (iterateScoutPaths grid)))

    scoutsInSamePlace scoutState = and [
        fst (pos (scout1 scoutState)) == fst (pos (scout2 scoutState)),
        snd (pos (scout1 scoutState)) == snd (pos (scout2 scoutState))]

    advanceScouts scoutState grid = ScoutState {
        scout1 = advanceScout (scout1 scoutState) grid,
        scout2 = advanceScout (scout2 scoutState) grid,
        dist = (dist scoutState) + 1
    }

    advanceScout scout grid = changeScoutDir (changeScoutPos scout grid) grid

    changeScoutPos scout grid = case dir scout of
        N -> Scout { pos = (fst (pos scout) - 1, snd (pos scout)), dir = dir scout }
        S -> Scout { pos = (fst (pos scout) + 1, snd (pos scout)), dir = dir scout }
        E -> Scout { pos = (fst (pos scout), snd (pos scout) + 1), dir = dir scout }
        W -> Scout { pos = (fst (pos scout), snd (pos scout) - 1), dir = dir scout }

    changeScoutDir scout grid = (case (getGridCell grid (pos scout)) of
        '|' -> Scout { pos = pos scout, dir = currentDir }
        '-' -> Scout { pos = pos scout, dir = currentDir }
        'F' -> Scout { pos = pos scout, dir = if currentDir == N then E else S }
        '7' -> Scout { pos = pos scout, dir = if currentDir == N then W else S }
        'J' -> Scout { pos = pos scout, dir = if currentDir == S then W else N }
        'L' -> Scout { pos = pos scout, dir = if currentDir == S then E else N }) where
        currentDir = dir scout

    findNoCellsInsidePipes :: Vector (Vector Char) -> [Coord] -> Int
    findNoCellsInsidePipes grid pipeLocations = sum (imap (\i x -> findNoCellsInsidePipesOnLine x i pipeLocations) grid)

    findNoCellsInsidePipesOnLine :: Vector Char -> Int -> [Coord] -> Int
    findNoCellsInsidePipesOnLine line lineIndex pipeLocations =
        fst (ifoldl (\a i x -> processCellForParity a i x lineIndex pipeLocations) (0, False) line)

    processCellForParity :: (Int, Bool) -> Int -> Char -> Int -> [Coord] -> (Int, Bool)
    processCellForParity state charIndex char lineIndex pipeLocations = (count, inside) where
        currentInside = snd state
        currentCount = fst state
        inside = if and[cellPartOfPipes, or[char == '|', char == 'F', char == '7']] then not currentInside else currentInside
        count = if and[not(cellPartOfPipes), inside] then currentCount + 1 else currentCount
        cellPartOfPipes = cellIsPartOfPipes pipeLocations (lineIndex, charIndex)

    getPipeCharAtCell :: Vector (Vector Char) -> [Coord] -> Coord -> Char
    getPipeCharAtCell grid pipeLocations pos = if cellIsPartOfPipes pipeLocations pos
        then getGridCell grid pos
        else '.'

    cellIsPartOfPipes :: [Coord] -> Coord -> Bool
    cellIsPartOfPipes pipePositions pos = length (filter (\x -> and[fst x == fst pos, snd x == snd pos]) pipePositions) >= 1

    findStart :: Vector (Vector Char) -> Coord
    findStart grid = ifoldl (\a i x -> if x /= -1 then (i, x) else a) (-1, -1) (vmap findStartOnLine grid)

    findStartOnLine :: Vector Char -> Int
    findStartOnLine line = ifoldl (\a i x -> if x == 'S' then i else a) (-1) line

    findDirections :: Vector (Vector Char) -> Coord -> (Direction, Direction)
    findDirections grid startPos = ((phead possibleDirections), (phead (tail possibleDirections))) where
        possibleDirections = filter (\x -> case x of
            N -> canGoNorth grid startPos
            S -> canGoSouth grid startPos
            E -> canGoEast grid startPos
            W -> canGoWest grid startPos) [N, S, E, W]

    canGoNorth grid pos = or [pipe == '|', pipe == 'F', pipe == '7']
        where pipe = getGridCell grid ((fst pos) - 1, snd pos)

    canGoSouth grid pos = or [pipe == '|', pipe == 'J', pipe == 'L']
        where pipe = getGridCell grid ((fst pos) + 1, snd pos)

    canGoEast grid pos = or [pipe == '-', pipe == 'J', pipe == '7']
        where pipe = getGridCell grid (fst pos, (snd pos) + 1)

    canGoWest grid pos = or [pipe == '-', pipe == 'F', pipe == 'L']
        where pipe = getGridCell grid (fst pos, (snd pos) - 1)

    getGridWithoutStart :: Vector (Vector Char) -> Vector (Vector Char)
    getGridWithoutStart grid = newGrid where
        originalStartLine = getByIndex grid (fst startPos)
        startPos = findStart grid
        newStartChar = case findDirections grid startPos of
            (N, S) -> '|'
            (N, E) -> 'L'
            (N, W) -> 'J'
            (S, E) -> 'F'
            (S, W) -> '7'
            (E, W) -> '-'
        newStartLine = originalStartLine // [((snd startPos), newStartChar)]
        newGrid = grid // [((fst startPos), newStartLine)]
