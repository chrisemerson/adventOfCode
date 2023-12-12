module Day9.Day9 where
    import Data.List.Split
    import Util

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ sum (map calculateNextNumber parsedInput) where
        parsedInput = parseInput input
    part2 input = show $ sum (map calculatePreviousNumber parsedInput) where
        parsedInput = parseInput input

    parseInput :: String -> [[Int]]
    parseInput input = map parseLine (filter (/= "") (map trim (lines input)))

    parseLine :: String -> [Int]
    parseLine line = map read (filter (/= "") (map trim (splitOn " " line)))

    calculateNextNumber :: [Int] -> Int
    calculateNextNumber sequence = if allEqual sequence
        then head sequence
        else (last sequence) + (calculateNextNumber (getDifferences sequence))

    calculatePreviousNumber :: [Int] -> Int
    calculatePreviousNumber sequence = if allEqual sequence
        then last sequence
        else (head sequence) - (calculatePreviousNumber (getDifferences sequence))

    getDifferences :: [Int] -> [Int]
    getDifferences sequence = fst (foldl (\a x -> ((fst a) ++ [x - snd a], x)) ([], head sequence) (tail sequence))

    allEqual :: [Int] -> Bool
    allEqual lst = snd (foldl (\a x -> (x, and[snd a, (x == fst a)])) ((head lst), True) lst)
