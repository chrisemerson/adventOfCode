module Day3.Day3 where
    import Data.Char
    import Data.Typeable
    import Data.Vector (fromList, ifoldl, imap, ifilter, Vector, (!), toList)
    import Util

    data Number = Number {row :: Int, start :: Int, end :: Int, number :: Int} deriving (Show)
    data ParseState = ParseState {numbers :: [Number], currentlyInDigit :: Bool, startPos :: Int, curNumber :: Int} deriving (Show)

    part1 :: String -> String
    part2 :: String -> String

    part1 input = sumEnginePartNumbers input
    part2 input = sumEnginePartNumbers input

    sumEnginePartNumbers input =
        show $ sum (map (\x -> number x) (filter (\x -> hasAdjacentSymbol x inputGrid) (findNumbers inputGrid))) where
            inputGrid = getInputGrid input

    getInputGrid :: String -> Vector (Vector Char)
    getInputGrid input = fromList (map (fromList . trim) (lines input))

    findNumbers inputVec = concat $ imap findNumbersInRow inputVec

    findNumbersInRow rowNo lineVec = numbers (ifoldl
        (\a i c -> updateParseState a c rowNo i)
        ParseState {numbers = [], currentlyInDigit = False, startPos = 0, curNumber = 0}
        lineVec)

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

    hasAdjacentSymbol :: Number -> Vector (Vector Char) -> Bool
    hasAdjacentSymbol number inputGrid = hasSymbol adjacentCells where
        adjacentCells = selectAdjacentCells (row number) (start number) (end number) inputGrid

    hasSymbol :: Vector (Vector Char) -> Bool
    hasSymbol cells = or (imap (\i x -> hasSymbolInRow x) cells)

    hasSymbolInRow :: Vector Char -> Bool
    hasSymbolInRow cells = or (imap (\i x -> not (or [isDigit x, x == '.'])) cells)

    selectAdjacentCells :: Int -> Int -> Int -> Vector (Vector Char) -> Vector (Vector Char)
    selectAdjacentCells rowNo start end inputGrid =
        imap (\x y -> selectAdjacentColumns start end y) (ifilter (\x y -> (abs (rowNo - x)) <= 1) inputGrid)

    selectAdjacentColumns :: Int -> Int -> Vector Char -> Vector Char
    selectAdjacentColumns start end row = ifilter (\x y -> and [(x >= (start - 1)), (x <= (end + 1))]) row
