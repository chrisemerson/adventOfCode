module Day13.Day13 where
    import Data.List.Split
    import Grid
    import Util

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ score where
        score = foldl (\a x -> a + (sum (fst x)) + (100 * (sum (snd x)))) 0 reflectionPoints
        reflectionPoints = map (\x -> (checkColsForReflection x, checkRowsForReflection x)) parsedInput
        parsedInput = parseInput input

    part2 input = input

    parseInput :: String -> [[[Char]]]
    parseInput input = map lines (splitOn "\n\n" input)

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
