module Day15.Day15 where
    import Data.Char
    import Data.List.Split
    import Util

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ sum (map hash (splitOn "," (trim input)))
    part2 input = input

    hash str = (foldl (\a x -> (a + (ord x)) * 17) 0 str) `mod` 256