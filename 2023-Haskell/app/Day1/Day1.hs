module Day1.Day1 where
    import Data.Char
    import Text.Regex.PCRE

    part1 :: String -> String
    part2 :: String -> String

    findDigit :: String -> String

    part1 input = show $ calibrationValueSum input calibrationValue
    part2 input = show $ calibrationValueSum input calibrationValueWithWords

    calibrationValueSum input calValueFn = sum (map calValueFn (lines input))

    calibrationValue line = 10 * (read [head digits]) + (read [last digits]) where
        digits = filter isDigit line

    calibrationValueWithWords line = 10 * (head digits) + (last digits) where
        digits = readTextDigits line

    readTextDigits line = map convertToDigit (filter (\x -> x /= "") (map findDigit (getLineSubstrings line)))

    getLineSubstrings str = map(\x -> drop x str) [0..((length str) - 1)]

    findDigit line = line =~ "^(1|2|3|4|5|6|7|8|9|0|one|two|three|four|five|six|seven|eight|nine|zero)"

    convertToDigit x = case x of
        "one" -> 1
        "two" -> 2
        "three" -> 3
        "four" -> 4
        "five" -> 5
        "six" -> 6
        "seven" -> 7
        "eight" -> 8
        "nine" -> 9
        "zero" -> 0
        "1" -> 1
        "2" -> 2
        "3" -> 3
        "4" -> 4
        "5" -> 5
        "6" -> 6
        "7" -> 7
        "8" -> 8
        "9" -> 9
        "0" -> 0
