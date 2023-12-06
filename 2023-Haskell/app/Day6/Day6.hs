module Day6.Day6 where
    import Data.List.Split
    import Util

    data CurrentRecord = CurrentRecord { time :: Int, distance :: Int } deriving (Show)

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ getWaysToWin records where
        records = parseInput input False

    part2 input = show $ getWaysToWin records where
        records = parseInput input True

    parseInput :: String -> Bool -> [CurrentRecord]
    parseInput input concatRecords = zipWith currentRecordFn (head inputNumbers) (head (tail inputNumbers)) where
        inputNumbers = if concatRecords then (map (\x -> [concat x]) (getInputNumbers input)) else (getInputNumbers input)
        currentRecordFn = (\t d -> CurrentRecord { time = read t, distance = read d })

    getInputNumbers :: String -> [[String]]
    getInputNumbers input = (map (\x -> filter (\x -> x /= "") (map trim (splitOn " " x))) inputLines) where
        inputLines = map (\x -> trim (drop 9 x)) (lines input)

    getWaysToWin :: [CurrentRecord] -> Int
    getWaysToWin records = product (map getWaysToBeatRecord records)

    getWaysToBeatRecord :: CurrentRecord -> Int
    getWaysToBeatRecord record = length (filter (\x -> x > (distance record)) distancesPerTime) where
        distancesPerTime = (map (calculateDistance (time record)) (range 0 (time record)))

    calculateDistance totalTime timeButtonHeld = (totalTime - timeButtonHeld) * timeButtonHeld