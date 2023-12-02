module Day2.Day2 where
    import Day2.Game
    import Data.Char (isSpace)
    import Data.List.Split

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ findSumOfValidGameIDs (parseInput input) 12 13 14
    part2 input = show $ sum (map getGamePower (parseInput input))

    parseInput input = map parseGame (lines input)

    parseGame line = Game {
        gameNo = read (drop 5 (head gameParts)),
        draws = parseDraws (last gameParts)
    } where
        gameParts = splitOn ":" line

    parseDraws drawsString = map parseDraw (splitOn ";" (trim drawsString))

    parseDraw drawString = Draw {
        red = (getCubes (trim drawString) "red"),
        green = (getCubes (trim drawString) "green"),
        blue = (getCubes (trim drawString) "blue")
    }

    getCubes drawString cubeColour = if ((length cubesString) == 0)
        then 0
        else read (trim (takeWhile (\x -> not (isSpace x)) (head cubesString))) where
        cubesString = getCubeColour drawString cubeColour

    getCubeStrings drawString = map trim (splitOn "," drawString)

    getCubeColour drawString cubeColour = filter (\x -> stringEndsWith x cubeColour) (getCubeStrings drawString)

    stringEndsWith str search = (drop ((length str) - (length search)) str) == search

    trim = f . f where
        f = reverse . dropWhile isSpace

    findSumOfValidGameIDs games maxRed maxGreen maxBlue =
        sum $ map (\x -> gameNo x) (filter (\x -> isValidGame x maxRed maxGreen maxBlue) games)

    isValidGame game maxRed maxGreen maxBlue =
        length (draws game) == (length (filter id (map (\x -> isValidDraw x maxRed maxGreen maxBlue) (draws game))))

    isValidDraw draw maxRed maxGreen maxBlue = and [red draw <= maxRed, green draw <= maxGreen, blue draw <= maxBlue]

    getGamePower game = (getMinimumRedCubes game) * (getMinimumGreenCubes game) * (getMinimumBlueCubes game)

    getMinimumRedCubes game = maximum (map red (draws game))
    getMinimumGreenCubes game = maximum (map green (draws game))
    getMinimumBlueCubes game = maximum (map blue (draws game))
