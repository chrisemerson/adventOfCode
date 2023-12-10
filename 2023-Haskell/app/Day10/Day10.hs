module Day10.Day10 where
    import Data.Vector (fromList, head, Vector, ifoldl, ifilter, imap, map, empty)
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
        grid = getInputGrid input

    part2 input = input

    getInputGrid :: String -> Vector (Vector Char)
    getInputGrid input = fromList (pmap (\x -> fromList (trim x)) (lines input))

    getInitialScoutState grid = ScoutState {
            scout1 = Scout { pos = startPos, dir = fst directions},
            scout2 = Scout { pos = startPos, dir = snd directions},
            dist = 0
        } where
        directions = findDirections grid startPos
        startPos = findStart grid

    findFurthestPipeDistance grid = dist (phead (dropWhile (not.scoutsInSamePlace) (iterate (\x -> advanceScouts x grid) initialState))) where
        initialState = advanceScouts (getInitialScoutState grid) grid

    scoutsInSamePlace scoutState = and [
        fst (pos (scout1 scoutState)) == fst (pos (scout2 scoutState)),
        snd (pos (scout1 scoutState)) == snd (pos (scout2 scoutState))]

    advanceScouts scoutState grid = ScoutState {
        scout1 = advanceScout (scout1 scoutState) grid,
        scout2 = advanceScout (scout2 scoutState) grid,
        dist = (dist scoutState) + 1
    }

    advanceScout scout grid = changeScoutDir (case dir scout of
        N -> Scout { pos = (fst (pos scout) - 1, snd (pos scout)), dir = dir scout }
        S -> Scout { pos = (fst (pos scout) + 1, snd (pos scout)), dir = dir scout }
        E -> Scout { pos = (fst (pos scout), snd (pos scout) + 1), dir = dir scout }
        W -> Scout { pos = (fst (pos scout), snd (pos scout) - 1), dir = dir scout }) grid

    changeScoutDir scout grid = (case (getGridChar grid (fst (pos scout)) (snd (pos scout))) of
        '|' -> Scout { pos = pos scout, dir = currentDir }
        '-' -> Scout { pos = pos scout, dir = currentDir }
        'F' -> Scout { pos = pos scout, dir = if currentDir == N then E else S }
        '7' -> Scout { pos = pos scout, dir = if currentDir == N then W else S }
        'J' -> Scout { pos = pos scout, dir = if currentDir == S then W else N }
        'L' -> Scout { pos = pos scout, dir = if currentDir == S then E else N }) where
        currentDir = dir scout

    findStart :: Vector (Vector Char) -> (Int, Int)
    findStart grid = ifoldl (\a i x -> if x /= -1 then (i, x) else a) (-1, -1) (vmap findStartOnLine grid)

    findStartOnLine :: Vector Char -> Int
    findStartOnLine line = ifoldl (\a i x -> if x == 'S' then i else a) (-1) line

    findDirections :: Vector (Vector Char) -> (Int, Int) -> (Direction, Direction)
    findDirections grid startPos = ((phead possibleDirections), (phead (tail possibleDirections))) where
        possibleDirections = filter (\x -> case x of
            N -> canGoNorth grid startPos
            S -> canGoSouth grid startPos
            E -> canGoEast grid startPos
            W -> canGoWest grid startPos) [N, S, E, W]

    canGoNorth grid pos = or [pipe == '|', pipe == 'F', pipe == '7']
        where pipe = getGridChar grid (fst pos - 1) (snd pos)
    canGoSouth grid pos = or [pipe == '|', pipe == 'J', pipe == 'L']
        where pipe = getGridChar grid (fst pos + 1) (snd pos)
    canGoEast grid pos = or [pipe == '-', pipe == 'J', pipe == '7']
        where pipe = getGridChar grid (fst pos) (snd pos + 1)
    canGoWest grid pos = or [pipe == '-', pipe == 'F', pipe == 'L']
        where pipe = getGridChar grid (fst pos) (snd pos - 1)

    getGridChar :: Vector (Vector Char) -> Int -> Int -> Char
    getGridChar grid y x = getByIndex (getByIndex grid y) x

    getByIndex :: Vector a -> Int -> a
    getByIndex vector idx = vhead (ifilter (\i x -> i == idx) vector)