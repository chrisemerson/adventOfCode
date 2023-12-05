module Day5.Almanac where
    data Map = Map { destStart :: Int, sourceStart :: Int, rangeSize :: Int} deriving (Show)
    data Almanac = Almanac {
        seeds :: [Int],
        seedToSoil :: [Map],
        soilToFertilizer :: [Map],
        fertilizerToWater :: [Map],
        waterToLight :: [Map],
        lightToTemperature :: [Map],
        temperatureToHumidity :: [Map],
        humidityToLocation :: [Map]
    } deriving (Show)
