module Day18.Day18 where
    import Data.List.Split
    import Util

    part1 :: String -> String
    part2 :: String -> String

    data ParseState = ParseState { vtcs :: [(Int, Int)], lastVertex :: (Int, Int) } deriving (Show, Eq)

    part1 input = show $ vertices where
        vertices = parseInput input

    part2 input = show $ input

    parseInput :: String -> [(Int, Int)]
    parseInput input = [last vertices] ++ vertices where
        vertices = vtcs (foldl parseLine ParseState { vtcs = [], lastVertex = (0, 0) } (lines input))

    parseLine :: ParseState -> String -> ParseState
    parseLine state line = ParseState {
        vtcs = (vtcs state) ++ [newVertex],
        lastVertex = newVertex
    } where
        newVertex = (case directionChar of
            'U' -> (lastY - distance, lastX)
            'D' -> (lastY + distance, lastX)
            'L' -> (lastY, lastX - distance)
            'R' -> (lastY, lastX + distance))
        last = (lastVertex state)
        lastX = snd last
        lastY = fst last
        distance = read (head (drop 1 lineParts)) :: Int
        directionChar = head (head lineParts)
        lineParts = splitOn " " (trim line)
