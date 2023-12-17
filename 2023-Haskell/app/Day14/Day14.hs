module Day14.Day14 where
    import Grid
    import Data.List (group)
    import Util

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ calculateStress gridAfterRolls where
        gridAfterRolls = rollRocksNorth grid
        grid = convertToGrid input

    part2 input = show $ map calculateStress (take 200 (iterate cycleRocks grid)) where
       grid = convertToGrid input

    cycleRocks :: Grid -> Grid
    cycleRocks grid = rollRocksEast (rollRocksSouth (rollRocksWest (rollRocksNorth grid)))

    rollRocksNorth grid = rollRocks grid findWhereRockCanRollToNorth findRocksThatCanRollNorth
    rollRocksWest grid = rollRocks grid findWhereRockCanRollToWest findRocksThatCanRollWest
    rollRocksSouth grid = rollRocks grid findWhereRockCanRollToSouth findRocksThatCanRollSouth
    rollRocksEast grid = rollRocks grid findWhereRockCanRollToEast findRocksThatCanRollEast

    rollRocks :: Grid -> (Grid -> Int -> Int -> (Int, Int)) -> (Grid -> [(Int, Int)]) -> Grid
    rollRocks grid getSpaceFn findFn = if not (length (findFn grid) > 0)
        then grid
        else rollRocks gridAfterRolling getSpaceFn findFn where
            gridAfterRolling = foldl (\a r -> swapGridChars a r (getSpaceFn grid (fst r) (snd r))) grid (findFn grid)

    findRocks grid = filter (\c -> getGridChar grid (fst c) (snd c) == 'O') cells where
        cells = concat $ map (\r -> map (\c -> (r, c)) (range 0 ((width grid) - 1))) (range 0 ((height grid) - 1))

    findRocksThatCanRollNorth grid = filter (\r -> and[fst r > 0, getGridChar grid ((fst r) - 1) (snd r) == '.']) (findRocks grid)
    findRocksThatCanRollWest grid = filter (\r -> and[snd r > 0, getGridChar grid (fst r) ((snd r) - 1) == '.']) (findRocks grid)
    findRocksThatCanRollSouth grid = filter (\r -> and[fst r < ((height grid) - 1), getGridChar grid ((fst r) + 1) (snd r) == '.']) (findRocks grid)
    findRocksThatCanRollEast grid = filter (\r -> and[snd r < ((width grid) - 1), getGridChar grid (fst r) ((snd r) + 1) == '.']) (findRocks grid)

    findWhereRockCanRollToNorth grid y x = (newY, x) where
        newY = y - (length (head (group (map (\n -> getGridChar grid n x) (stepRange (y - 1) 0 (-1))))))

    findWhereRockCanRollToWest grid y x = (y, newX) where
        newX = x - (length (head (group (map (\n -> getGridChar grid y n) (stepRange (x - 1) 0 (-1))))))

    findWhereRockCanRollToSouth grid y x = (newY, x) where
        newY = y + (length (head (group (map (\n -> getGridChar grid n x) (range (y + 1) ((height grid) - 1))))))

    findWhereRockCanRollToEast grid y x = (y, newX) where
        newX = x + (length (head (group (map (\n -> getGridChar grid y n) (range (x + 1) ((width grid) -1))))))

    calculateStress grid = sum stressPerRow where
        stressPerRow = gmap (\i r -> ((height grid) - i) * (glength (gfilter (\_ c -> c == 'O') r))) grid
