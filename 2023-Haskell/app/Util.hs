module Util where
    import Data.Char (isSpace)

    trim :: String -> String
    trim = f . f where
        f = reverse . dropWhile isSpace

    stringStartsWith :: String -> String -> Bool
    stringStartsWith str search = (take (length search) str) == search

    stringEndsWith :: String -> String -> Bool
    stringEndsWith str search = (drop ((length str) - (length search)) str) == search

    stringContains :: String -> String -> Bool
    stringContains str search = if stringStartsWith str search then True else stringContains (drop 1 str) search

    range :: Int -> Int -> [Int]
    range start end = take (end - start + 1) (iterate (+ 1) start)

    frequencies :: String -> [(Char, Int)]
    frequencies string = [ (x, c) | x <- ['A'..'z'] ++ ['0'..'9'], let c = (length.filter (== x)) string, c > 0 ]

    rmdups :: Eq a => [a] -> [a]
    rmdups [] = []
    rmdups (x:xs) = x : filter (/= x) (rmdups xs)

    replaceCharInString :: String -> Int -> Char -> String
    replaceCharInString str idx char = (take idx str) ++ [char] ++ (drop (idx + 1) str)