module Day18.Day18 where
    import Data.Char (digitToInt, toUpper)
    import Data.List.Split
    import Util

    part1 :: String -> String
    part2 :: String -> String

    data ParseState = ParseState { vtcs :: [(Int, Int)], lastVertex :: (Int, Int), totalDistance :: Int } deriving (Show, Eq)

    part1 input = show $ calculateArea (parseInput input)
    part2 input = show $ calculateArea (parseInput2 input)

    calculateArea :: ParseState -> Int
    calculateArea parsedInput = (shoelace vertices) + (round (fromIntegral (totalDistance parsedInput) / 2)) + 1 where
        vertices = [last parsedVertices] ++ parsedVertices
        parsedVertices = vtcs parsedInput

    parseInput :: String -> ParseState
    parseInput input = foldl parseLine ParseState { vtcs = [], lastVertex = (0, 0), totalDistance = 0 } (lines input)

    parseInput2 :: String -> ParseState
    parseInput2 input = foldl parseLine2 ParseState { vtcs = [], lastVertex = (0, 0), totalDistance = 0 } (lines input)

    parseLine :: ParseState -> String -> ParseState
    parseLine state line = ParseState {
        vtcs = (vtcs state) ++ [newVertex],
        lastVertex = newVertex,
        totalDistance = (totalDistance state) + distance
    } where
        newVertex = (case directionChar of
            'U' -> (lastY - distance, lastX)
            'D' -> (lastY + distance, lastX)
            'L' -> (lastY, lastX - distance)
            'R' -> (lastY, lastX + distance))
        last = lastVertex state
        lastX = snd last
        lastY = fst last
        distance = read (head (drop 1 lineParts)) :: Int
        directionChar = head (head lineParts)
        lineParts = splitOn " " (trim line)

    parseLine2 :: ParseState -> String -> ParseState
    parseLine2 state line = ParseState {
        vtcs = (vtcs state) ++ [newVertex],
        lastVertex = newVertex,
        totalDistance = (totalDistance state) + distance
    } where
        newVertex = (case directionChar of
            '0' -> (lastY, lastX + distance)
            '1' -> (lastY + distance, lastX)
            '2' -> (lastY, lastX - distance)
            '3' -> (lastY - distance, lastX))
        lastV = lastVertex state
        lastX = snd lastV
        lastY = fst lastV
        distance = hexToDecimal (take 5 colour)
        directionChar = head (drop 5 colour)
        colour = take 6 (drop 2 (last lineParts))
        lineParts = splitOn " " (trim line)

    hexToDecimal :: String -> Int
    hexToDecimal = sum . zipWith (*) (iterate (*16) 1) . reverse . map digitToInt . map toUpper

    shoelace :: [(Int, Int)] -> Int
    shoelace vertices = round ((fromIntegral (abs (sum1 - sum2))) / 2) where
        (sum1, sum2, _, _) = foldl processShoelaceStep (0, 0, fst (head vertices), snd (head vertices)) vertices

    processShoelaceStep :: (Int, Int, Int, Int) -> (Int, Int) -> (Int, Int, Int, Int)
    processShoelaceStep (sum1, sum2, lastY, lastX) (thisY, thisX) = (sum1 + (thisY * lastX), sum2 + (thisX * lastY), thisY, thisX)