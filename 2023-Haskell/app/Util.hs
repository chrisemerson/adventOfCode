module Util where
    import Data.Char (isSpace)

    trim :: String -> String
    trim = f . f where
        f = reverse . dropWhile isSpace

    stringStartsWith :: String -> String -> Bool
    stringStartsWith str search = (take (length search) str) == search

    stringEndsWith :: String -> String -> Bool
    stringEndsWith str search = (drop ((length str) - (length search)) str) == search

    range :: Int -> Int -> [Int]
    range start end = take (end - start + 1) (iterate (+ 1) start)