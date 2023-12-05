module Day5.Almanac where
    data SeedRange = SeedRange { startNo :: Int, size :: Int } deriving (Show)
    data Map = Map { destStart :: Int, sourceStart :: Int, rangeSize :: Int} deriving (Show)
    data Almanac = Almanac {
        seeds :: [SeedRange],
        seedToSoil :: [Map],
        soilToFertilizer :: [Map],
        fertilizerToWater :: [Map],
        waterToLight :: [Map],
        lightToTemperature :: [Map],
        temperatureToHumidity :: [Map],
        humidityToLocation :: [Map]
    } deriving (Show)
