module Day5.Day5 where
    import Day5.Almanac
    import Day5.InputParser (parseInput)
    import Util

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ findLowestLocation (parseInput input False)
    part2 input = show $ findSeedForLowestLocation (parseInput input True)

    findLowestLocation :: Almanac -> Int
    findLowestLocation almanac = minimum (map (\x -> minimum (map (findLocationForSeed almanac) (getSeedRange x))) (seeds almanac))

    getSeedRange :: SeedRange -> [Int]
    getSeedRange seedRange = take (size seedRange) (iterate (+ 1) (startNo seedRange))

    findSeedForLowestLocation almanac = head (dropWhile
        (\x -> (length (filter (\y -> seedIsInAlmanac y almanac) (findSeedsForLocation almanac x))) == 0)
        (iterate (+ 1) 0))

    findLocationForSeed :: Almanac -> Int -> Int
    findLocationForSeed almanac seedNo =
        lookupInAlmanac almanac humidityToLocation
        $ lookupInAlmanac almanac temperatureToHumidity
        $ lookupInAlmanac almanac lightToTemperature
        $ lookupInAlmanac almanac waterToLight
        $ lookupInAlmanac almanac fertilizerToWater
        $ lookupInAlmanac almanac soilToFertilizer
        $ lookupInAlmanac almanac seedToSoil
        seedNo

    findSeedsForLocation :: Almanac -> Int -> [Int]
    findSeedsForLocation almanac locationNo =
        reverseLookupInAlmanac almanac seedToSoil
        $ reverseLookupInAlmanac almanac soilToFertilizer
        $ reverseLookupInAlmanac almanac fertilizerToWater
        $ reverseLookupInAlmanac almanac waterToLight
        $ reverseLookupInAlmanac almanac lightToTemperature
        $ reverseLookupInAlmanac almanac temperatureToHumidity
        $ reverseLookupInAlmanac almanac humidityToLocation
        [locationNo]

    seedIsInAlmanac :: Int -> Almanac -> Bool
    seedIsInAlmanac seedNo almanac = 1 == (length (filter (\x -> and [seedNo < ((startNo x) + (size x)), seedNo >= (startNo x)]) (seeds almanac)))

    lookupInAlmanac :: Almanac -> (Almanac -> [Map]) -> Int -> Int
    lookupInAlmanac almanac function value = if (length correctMap == 1)
        then lookupValueInMap value (head correctMap)
        else value
        where
            correctMap = filter (\x -> mapMapsValue x value) (function almanac)

    reverseLookupInAlmanac :: Almanac -> (Almanac -> [Map]) -> [Int] -> [Int]
    reverseLookupInAlmanac almanac function values = concat (map (reverseLookupSingleValueInAlmanac almanac function) values)

    reverseLookupSingleValueInAlmanac :: Almanac -> (Almanac -> [Map]) -> Int -> [Int]
    reverseLookupSingleValueInAlmanac almanac function value = if (length correctMaps == 0)
        then [value]
        else (reverseLookupValueInMaps value correctMaps)
        where
            correctMaps = filter(\x -> mapReverseMapsValue x value) (function almanac)

    mapMapsValue :: Map -> Int -> Bool
    mapMapsValue lookupMap value = and [
        value >= (sourceStart lookupMap),
        value <= ((sourceStart lookupMap) + (rangeSize lookupMap))]

    mapReverseMapsValue :: Map -> Int -> Bool
    mapReverseMapsValue lookupMap value = and [
        value >= (destStart lookupMap),
        value <= ((destStart lookupMap) + (rangeSize lookupMap))]

    lookupValueInMap value lookupMap = value - (sourceStart lookupMap) + (destStart lookupMap)

    reverseLookupValueInMaps value lookupMaps = map (\m -> value - (destStart m) + (sourceStart m)) lookupMaps
