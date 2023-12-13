module Day13.Day13 where
    import Data.List
    import Data.List.Split
    import Grid
    import Util

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ scoreReflectionPoints (map findReflectionPoints (parseInput input))

    part2 input = show $ scoreReflectionPoints (map findReflectionPointsAfterSmudgeCorrection (parseInput input))

    parseInput :: String -> [[[Char]]]
    parseInput input = map lines (splitOn "\n\n" input)

    scoreReflectionPoints reflectionPoints = foldl (\a x -> a + (sum (fst x)) + (100 * (sum (snd x)))) 0 reflectionPoints

    findReflectionPoints :: [[Char]] -> ([Int], [Int])
    findReflectionPoints grid = (checkColsForReflection grid, checkRowsForReflection grid)

    checkColsForReflection :: [[Char]] -> [Int]
    checkColsForReflection grid =
        foldl (\a x -> if checkColForReflection grid x then a ++ [x] else a) [] (range 1 (length (head grid)))

    checkColForReflection :: [[Char]] -> Int -> Bool
    checkColForReflection grid colNo = length (filter (not . id) (map (\v -> checkListForReflection v colNo) grid)) == 0

    checkRowsForReflection :: [[Char]] -> [Int]
    checkRowsForReflection grid =
        foldl (\a x -> if checkRowForReflection grid x then a ++ [x] else a) [] (range 1 (length grid))

    checkRowForReflection :: [[Char]] -> Int -> Bool
    checkRowForReflection grid rowNo = checkListForReflection grid rowNo

    checkListForReflection lst splitPoint = if splitPoint == (length lst)
        then False
        else checkListForReflectionDownMiddle (take noToTake (drop noToDrop lst)) where
            noToTake = 2 * noToCheck
            noToDrop = max 0 (splitPoint - noToCheck)
            noToCheck = min splitPoint ((length lst) - splitPoint)

    checkListForReflectionDownMiddle lst = if (length lst) == 0
        then True
        else if (head lst == last lst)
            then checkListForReflectionDownMiddle (take ((length lst) - 2) (drop 1 lst))
            else False

    findReflectionPointsAfterSmudgeCorrection :: [[Char]] -> ([Int], [Int])
    findReflectionPointsAfterSmudgeCorrection grid = (filter (\x -> not (elem x (fst originalReflectionLines))) (fst nonOriginalNewReflections), filter (\x -> not (elem x (snd originalReflectionLines))) (snd nonOriginalNewReflections)) where
        nonOriginalNewReflections = head (filter (\x -> or[(fst x) /= (fst originalReflectionLines), (snd x) /= (snd originalReflectionLines)]) newReflectionsPerCharChange)
        newReflectionsPerCharChange = filter (\x -> or [length (fst x) /= 0, length (snd x) /= 0]) (map findReflectionPoints newGridsPerCharChange)
        newGridsPerCharChange = foldl (\a x -> a ++ x) [] (map (\i -> (map (\j -> swapCharacterInGrid grid i j) (range 0 gridNoCols))) (range 0 gridNoRows))
        gridNoRows = (length grid) - 1
        gridNoCols = (length (head grid)) - 1
        originalReflectionLines = findReflectionPoints grid

    swapCharacterInGrid :: [[Char]] -> Int -> Int -> [[Char]]
    swapCharacterInGrid grid row col = replacementGrid where
        replacementGrid = rowsBefore ++ [replacementRow] ++ rowsAfter
        rowToReplace = head (drop row grid)
        rowsBefore = take row grid
        rowsAfter = drop (row + 1) grid
        replacementRow = colsBefore ++ [replacementChar] ++ colsAfter
        colsBefore = take col rowToReplace
        colsAfter = drop (col + 1) rowToReplace
        originalChar = head (drop col rowToReplace)
        replacementChar = if originalChar == '#' then '.' else '#'
