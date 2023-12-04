module Day4.Day4 where
    import Data.List
    import Data.List.Split
    import Data.Vector (Vector, empty, ifoldl, generate, (//), (!))
    import Util

    part1 :: String -> String
    part2 :: String -> String

    data Card = Card { cardNo :: Int, winningNumbers :: [Int], ourNumbers :: [Int] } deriving (Show)

    part1 input = show $ getAllScratchcardPoints (parseCards input)
    part2 input = show $ determineNumberOfScratchcards (parseCards input)

    parseCards input = map parseCard (lines input)

    parseCard line = Card {
        cardNo = read (trim (take 3 (drop 5 line))),
        winningNumbers = map read (filter (\x -> x /= "") (map (trim) (splitOn " " (take 31 (drop 9 line))))),
        ourNumbers = map read (filter (\x -> x /= "") (map (trim) (splitOn " " (drop 42 line))))
    }

    getAllScratchcardPoints scratchcards = sum (map getScratchcardPoints scratchcards)

    getScratchcardPoints scratchcard = if matchingNumbers == 0 then 0 else 2 ^ (matchingNumbers - 1) where
        matchingNumbers = getNumberOfMatchingNumbersOnScratchcard scratchcard

    getNumberOfMatchingNumbersOnScratchcard :: Card -> Int
    getNumberOfMatchingNumbersOnScratchcard scratchcard =
        length (intersect (winningNumbers scratchcard) (ourNumbers scratchcard))

    determineNumberOfScratchcards :: [Card] -> Int
    determineNumberOfScratchcards scratchcards = ifoldl
        (\a i x -> a + x)
        0
        (foldl processScratchcard (generate (length scratchcards) (\a -> 1)) scratchcards)

    processScratchcard :: Vector Int -> Card -> Vector Int
    processScratchcard currentQuantities scratchcard = currentQuantities // quantitiesToIncrease where
        cardIndex = (cardNo scratchcard) - 1
        matchingNumbers = getNumberOfMatchingNumbersOnScratchcard scratchcard
        quantitiesToIncrease = foldl
            (\a x -> a ++ [(x, (currentQuantities ! x) + (currentQuantities ! cardIndex))])
            ([] :: [(Int, Int)])
            (range (cardIndex + 1) (cardIndex + matchingNumbers))
