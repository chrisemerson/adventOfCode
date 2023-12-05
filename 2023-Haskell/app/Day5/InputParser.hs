module Day5.InputParser where
    import Data.List.Split
    import Day5.Almanac
    import Util

    data ParseState = ParseState { almanac :: Almanac, stage :: String } deriving (Show)

    parseInput :: String -> Bool -> Almanac
    parseInput input useSeedRanges = almanac (foldl (\a x -> parseLine a x useSeedRanges) ParseState {
            almanac = Almanac {
                seeds = [],
                seedToSoil = [],
                soilToFertilizer = [],
                fertilizerToWater = [],
                waterToLight = [],
                lightToTemperature = [],
                temperatureToHumidity = [],
                humidityToLocation = []
            },
            stage = ""
        } inputLines) where
            inputLines = filter (/= "") (map trim (lines input))

    parseLine :: ParseState -> String -> Bool -> ParseState
    parseLine parseState line useSeedRanges =
        if stringStartsWith line "seeds:"
            then ParseState { almanac = Almanac {
                seeds = if useSeedRanges then parseSeedsWithRanges line else parseSeeds line,
                seedToSoil = seedToSoil (almanac parseState),
                soilToFertilizer = seedToSoil (almanac parseState),
                fertilizerToWater = seedToSoil (almanac parseState),
                waterToLight = seedToSoil (almanac parseState),
                lightToTemperature = seedToSoil (almanac parseState),
                temperatureToHumidity = seedToSoil (almanac parseState),
                humidityToLocation = seedToSoil (almanac parseState)
            }, stage = "seeds"}
        else if line == "seed-to-soil map:"
            then ParseState { almanac = almanac parseState, stage = "seedToSoil"}
        else if line == "soil-to-fertilizer map:"
            then ParseState { almanac = almanac parseState, stage = "soilToFertilizer"}
        else if line == "fertilizer-to-water map:"
            then ParseState { almanac = almanac parseState, stage = "fertilizerToWater"}
        else if line == "water-to-light map:"
            then ParseState { almanac = almanac parseState, stage = "waterToLight"}
        else if line == "light-to-temperature map:"
            then ParseState { almanac = almanac parseState, stage = "lightToTemperature"}
        else if line == "temperature-to-humidity map:"
            then ParseState { almanac = almanac parseState, stage = "temperatureToHumidity"}
        else if line == "humidity-to-location map:"
            then ParseState { almanac = almanac parseState, stage = "humidityToLocation"}
        else if stage parseState == "seedToSoil"
            then ParseState {
                almanac = Almanac {
                    seeds = seeds (almanac parseState),
                    seedToSoil = seedToSoil (almanac parseState) ++ [readMapLine line],
                    soilToFertilizer = soilToFertilizer (almanac parseState),
                    fertilizerToWater = fertilizerToWater (almanac parseState),
                    waterToLight = waterToLight (almanac parseState),
                    lightToTemperature = lightToTemperature (almanac parseState),
                    temperatureToHumidity = temperatureToHumidity (almanac parseState),
                    humidityToLocation = humidityToLocation (almanac parseState)
                },
                stage = stage parseState
            }
        else if stage parseState == "soilToFertilizer"
            then ParseState {
                almanac = Almanac {
                    seeds = seeds (almanac parseState),
                    seedToSoil = seedToSoil (almanac parseState),
                    soilToFertilizer = soilToFertilizer (almanac parseState) ++ [readMapLine line],
                    fertilizerToWater = fertilizerToWater (almanac parseState),
                    waterToLight = waterToLight (almanac parseState),
                    lightToTemperature = lightToTemperature (almanac parseState),
                    temperatureToHumidity = temperatureToHumidity (almanac parseState),
                    humidityToLocation = humidityToLocation (almanac parseState)
                },
                stage = stage parseState
            }
        else if stage parseState == "fertilizerToWater"
            then ParseState {
                almanac = Almanac {
                    seeds = seeds (almanac parseState),
                    seedToSoil = seedToSoil (almanac parseState),
                    soilToFertilizer = soilToFertilizer (almanac parseState),
                    fertilizerToWater = fertilizerToWater (almanac parseState) ++ [readMapLine line],
                    waterToLight = waterToLight (almanac parseState),
                    lightToTemperature = lightToTemperature (almanac parseState),
                    temperatureToHumidity = temperatureToHumidity (almanac parseState),
                    humidityToLocation = humidityToLocation (almanac parseState)
                },
                stage = stage parseState
            }
        else if stage parseState == "waterToLight"
            then ParseState {
                almanac = Almanac {
                    seeds = seeds (almanac parseState),
                    seedToSoil = seedToSoil (almanac parseState),
                    soilToFertilizer = soilToFertilizer (almanac parseState),
                    fertilizerToWater = fertilizerToWater (almanac parseState),
                    waterToLight = waterToLight (almanac parseState) ++ [readMapLine line],
                    lightToTemperature = lightToTemperature (almanac parseState),
                    temperatureToHumidity = temperatureToHumidity (almanac parseState),
                    humidityToLocation = humidityToLocation (almanac parseState)
                },
                stage = stage parseState
            }
        else if stage parseState == "lightToTemperature"
            then ParseState {
                almanac = Almanac {
                    seeds = seeds (almanac parseState),
                    seedToSoil = seedToSoil (almanac parseState),
                    soilToFertilizer = soilToFertilizer (almanac parseState),
                    fertilizerToWater = fertilizerToWater (almanac parseState),
                    waterToLight = waterToLight (almanac parseState),
                    lightToTemperature = lightToTemperature (almanac parseState) ++ [readMapLine line],
                    temperatureToHumidity = temperatureToHumidity (almanac parseState),
                    humidityToLocation = humidityToLocation (almanac parseState)
                },
                stage = stage parseState
            }
        else if stage parseState == "temperatureToHumidity"
            then ParseState {
                almanac = Almanac {
                    seeds = seeds (almanac parseState),
                    seedToSoil = seedToSoil (almanac parseState),
                    soilToFertilizer = soilToFertilizer (almanac parseState),
                    fertilizerToWater = fertilizerToWater (almanac parseState),
                    waterToLight = waterToLight (almanac parseState),
                    lightToTemperature = lightToTemperature (almanac parseState),
                    temperatureToHumidity = temperatureToHumidity (almanac parseState) ++ [readMapLine line],
                    humidityToLocation = humidityToLocation (almanac parseState)
                },
                stage = stage parseState
            }
        else if stage parseState == "humidityToLocation"
            then ParseState {
                almanac = Almanac {
                    seeds = seeds (almanac parseState),
                    seedToSoil = seedToSoil (almanac parseState),
                    soilToFertilizer = soilToFertilizer (almanac parseState),
                    fertilizerToWater = fertilizerToWater (almanac parseState),
                    waterToLight = waterToLight (almanac parseState),
                    lightToTemperature = lightToTemperature (almanac parseState),
                    temperatureToHumidity = temperatureToHumidity (almanac parseState),
                    humidityToLocation = humidityToLocation (almanac parseState) ++ [readMapLine line]
                },
                stage = stage parseState
            }
        else parseState

    parseSeeds :: String -> [SeedRange]
    parseSeeds line = map (\x -> SeedRange { startNo = read x, size = 1}) (readSeedsLine line)

    parseSeedsWithRanges :: String -> [SeedRange]
    parseSeedsWithRanges line = map (\x -> SeedRange { startNo = read (head x), size = read (head (tail x))}) (chunksOf 2 (readSeedsLine line))

    readSeedsLine line = (filter (/= "") (map trim (splitOn " " (drop 6 line))))

    readMapLine :: String -> Map
    readMapLine line = Map {
        destStart = head parts,
        sourceStart = head (drop 1 parts),
        rangeSize = head (drop 2 parts)
    } where parts = map read (map trim (splitOn " " line))