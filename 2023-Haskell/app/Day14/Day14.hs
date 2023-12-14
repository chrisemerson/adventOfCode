module Day14.Day14 where
    import Grid
    import Util

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ calculateStress gridAfterRolls where
        gridAfterRolls = rollRocksNorth grid
        grid = convertToGrid input

    part2 input = show $ swapGridChars grid (0, 0) (0, 1) where
       grid = convertToGrid input

    rollRocksNorth grid = rollRocks grid (-1) 0 findRocksThatCanRollNorth
    rollRocksWest grid = rollRocks grid 0 (-1) findRocksThatCanRollNorth
    rollRocksSouth grid = rollRocks grid 1 0 findRocksThatCanRollNorth
    rollRocksEast grid = rollRocks grid 0 1 findRocksThatCanRollNorth

    rollRocks :: Grid -> Int -> Int -> (Grid -> [(Int, Int)]) -> Grid
    rollRocks grid yOffset xOffset findFn = if not (length (findFn grid) > 0)
        then grid
        else rollRocks (swapGridChars grid rockToMove (((fst rockToMove) + yOffset), ((snd rockToMove) + xOffset))) yOffset xOffset findFn where
           rockToMove = head (findFn grid)

    findRocks grid = filter (\c -> getGridChar grid (fst c) (snd c) == 'O') cells where
        cells = concat $ map (\r -> map (\c -> (r, c)) (range 0 ((glength (ghead grid)) - 1))) (range 0 ((glength grid) - 1))

    findRocksThatCanRollNorth grid = filter (\r -> and[fst r > 0, getGridChar grid ((fst r) - 1) (snd r) == '.']) (findRocks grid)
    findRocksThatCanRollWest grid = filter (\r -> and[snd r > 0, getGridChar grid (fst r) ((snd r) - 1) == '.']) (findRocks grid)
    findRocksThatCanRollSouth grid = filter (\r -> and[fst r < ((glength grid) - 1), getGridChar grid ((fst r) + 1) (snd r) == '.']) (findRocks grid)
    findRocksThatCanRollEast grid = filter (\r -> and[snd r < ((glength (ghead grid)) - 1), getGridChar grid (fst r) ((snd r) + 1) == '.']) (findRocks grid)

    calculateStress grid = sum stressPerRow where
        stressPerRow = gmap (\i r -> (gridHeight - i) * (glength (gfilter (\_ c -> c == 'O') r))) grid
        gridHeight = glength grid
