module Day5.Day5 where
    import Day5.Almanac
    import Day5.InputParser (parseInput)
    import Util

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ minimum (map (findLocationForSeed almanac) (seeds almanac)) where
        almanac = parseInput input
    part2 input = show $ filter (\x -> mapMapsValue x 79) (seedToSoil (parseInput input))

    findLocationForSeed :: Almanac -> Int -> Int
    findLocationForSeed almanac seedNo =
        lookupInAlmanac almanac humidityToLocation
        $ lookupInAlmanac almanac temperatureToHumidity
        $ lookupInAlmanac almanac lightToTemperature
        $ lookupInAlmanac almanac waterToLight
        $ lookupInAlmanac almanac fertilizerToWater
        $ lookupInAlmanac almanac soilToFertilizer
        $ lookupInAlmanac almanac seedToSoil seedNo

    lookupInAlmanac :: Almanac -> (Almanac -> [Map]) -> Int -> Int
    lookupInAlmanac almanac function value = if (length correctMap == 1)
        then lookupValueInMap value (head correctMap)
        else value
        where
            correctMap = filter (\x -> mapMapsValue x value) (function almanac)

    mapMapsValue :: Map -> Int -> Bool
    mapMapsValue lookupMap value = and [
        value >= (sourceStart lookupMap),
        value <= ((sourceStart lookupMap) + (rangeSize lookupMap))]

    lookupValueInMap value lookupMap = value - (sourceStart lookupMap) + (destStart lookupMap) where
