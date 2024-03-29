module Day12.Day12 where
    import Data.List (group)
    import Data.List.Split
    import Data.List.Utils (join)
    import Util

    part1 :: String -> String
    part2 :: String -> String

    data SpringRow = SpringRow { row :: String, unknowns :: [Int], pattern :: [Int] } deriving (Show)

    part1 input = show $ sum (map getValidSpringCombinations parsedInput) where
        parsedInput = parseInput input

    part2 input = show $ sum (map getValidSpringCombinations parsedInput) where
        parsedInput = map (\s -> unfoldSpringRow s 5) (parseInput input)

    parseInput input = map parseLine (lines input)

    parseLine line = SpringRow { row = head lineParts, unknowns = springUnknowns, pattern = springPattern } where
        springPattern = map read (splitOn "," (head (tail lineParts)))
        springUnknowns = findUnknowns (head lineParts)
        lineParts = splitOn " " line

    findUnknowns springs = snd (foldl (\a x -> if x == '?' then ((fst a) + 1, (snd a) ++ [fst a]) else ((fst a) + 1, snd a)) (0, []) springs)

    type Memo f = f -> f

    getValidSpringCombinations :: SpringRow -> Int
    getValidSpringCombinations springRow = if length (unknowns springRow) == 0
        then if springRowIsValid springRow then 1 else 0
        else getValidSpringCombinations SpringRow { row = replaceCharInString (row springRow) (head (unknowns springRow)) '#', unknowns = tail (unknowns springRow), pattern = (pattern springRow) }
            + getValidSpringCombinations SpringRow { row = replaceCharInString (row springRow) (head (unknowns springRow)) '.', unknowns = tail (unknowns springRow), pattern = (pattern springRow) }

    springRowIsValid :: SpringRow -> Bool
    springRowIsValid springRow = pattern springRow == map length (filter (\x -> head x == '#') (group (row springRow)))

    unfoldSpringRow springRow multiple = SpringRow { row = newRow, unknowns = newUnknowns, pattern = newPattern } where
        newUnknowns = findUnknowns newRow
        newRow = join "?" (take multiple (repeat (row springRow)))
        newPattern = concat (take multiple (repeat (pattern springRow)))
