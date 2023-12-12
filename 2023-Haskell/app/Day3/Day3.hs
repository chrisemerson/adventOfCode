module Day3.Day3 where
    import Data.Char
    import Data.Typeable
    import Data.Vector (fromList, ifoldl, imap, ifilter, Vector, map)
    import Grid
    import Util

    vmap = Data.Vector.map
    pmap = Prelude.map

    data Number = Number {row :: Int, start :: Int, end :: Int, number :: Int} deriving (Show)
    data ParseState = ParseState {numbers :: [Number], currentlyInDigit :: Bool, startPos :: Int, curNumber :: Int} deriving (Show)
    data Gear = Gear {y :: Int, x :: Int} deriving (Show)

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ sumEnginePartNumbers input
    part2 input = show $ sumGearRatios input

    sumEnginePartNumbers input =
        sum (pmap number (filter (hasAdjacentSymbol inputGrid) (findNumbers inputGrid))) where
            inputGrid = convertToGrid input

    sumGearRatios input = sum (pmap product (filter (\x -> length x == 2) numbersAdjacentToGears)) where
        numbersAdjacentToGears = pmap (pmap number . (findNumbersAdjacentToGear numbers)) (findGears inputGrid)
        numbers = findNumbers inputGrid
        inputGrid = convertToGrid input

    findNumbers inputVec = concat $ imap findNumbersInRow inputVec

    findGears inputVec = concat $ imap findGearsInRow inputVec

    findNumbersInRow rowNo lineVec = numbers (ifoldl
        (\a i c -> updateParseState a c rowNo i)
        ParseState {numbers = [], currentlyInDigit = False, startPos = 0, curNumber = 0}
        lineVec)

    findGearsInRow rowNo lineVec = ifoldl (\a i c -> if c == '*' then a ++ [Gear {y = rowNo, x = i}] else a) [] lineVec

    updateParseState parseState nextChar rowNo pos = if isDigit nextChar
        then if currentlyInDigit parseState
            then ParseState {
                numbers = numbers parseState,
                currentlyInDigit = True,
                startPos = startPos parseState,
                curNumber = (read [nextChar]) + 10 * (curNumber parseState)
            }
            else ParseState {
                numbers = numbers parseState,
                currentlyInDigit = True,
                startPos = pos,
                curNumber = (read [nextChar])
            }
        else if currentlyInDigit parseState
            then ParseState {
                numbers = (numbers parseState) ++ [Number {
                    row = rowNo,
                    start = (startPos parseState),
                    end = pos - 1,
                    number = curNumber parseState
                }],
                currentlyInDigit = False,
                startPos = 0,
                curNumber = 0
            }
            else parseState

    hasAdjacentSymbol inputGrid number = hasSymbol adjacentCells where
        adjacentCells = selectAdjacentCells (row number) (start number) (end number) inputGrid

    hasSymbol :: Vector (Vector Char) -> Bool
    hasSymbol cells = or (vmap hasSymbolInRow cells)

    hasSymbolInRow :: Vector Char -> Bool
    hasSymbolInRow cells = or (imap (\i x -> not (or [isDigit x, x == '.'])) cells)

    selectAdjacentCells :: Int -> Int -> Int -> Vector (Vector Char) -> Vector (Vector Char)
    selectAdjacentCells rowNo start end inputGrid =
        imap (\x y -> selectAdjacentColumns start end y) (ifilter (\x y -> (abs (rowNo - x)) <= 1) inputGrid)

    selectAdjacentColumns :: Int -> Int -> Vector Char -> Vector Char
    selectAdjacentColumns start end row = ifilter (\x y -> and [(x >= (start - 1)), (x <= (end + 1))]) row

    findNumbersAdjacentToGear numbers gear = filter (numberIsAdjacentToGear gear) numbers

    numberIsAdjacentToGear gear number = and [
        abs ((y gear) - (row number)) <= 1,
        (x gear) >= ((start number) - 1),
        (x gear) <= ((end number) + 1)]
