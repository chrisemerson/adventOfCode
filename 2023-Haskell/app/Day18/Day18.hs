module Day18.Day18 where
    import Data.List.Split
    import Util

    part1 :: String -> String
    part2 :: String -> String

    data Colour = Colour { colour :: String } deriving (Show, Eq)
    data Direction = U | D | L | R deriving (Show, Eq)
    data Step = Step { direction :: Direction, distance :: Int, stepColour :: Colour } deriving (Show, Eq)
    data DugCell = DugCell { xPos :: Int, yPos :: Int, cellColour :: Colour } deriving (Show, Eq)
    data Field = Field { dugCells :: [DugCell], currentX :: Int, currentY :: Int } deriving (Show)

    part1 input = show $ findBoundingRectangle field where
        field = foldl processStep initialField steps
        steps = parseInput input

    part2 input = show $ input

    parseInput :: String -> [Step]
    parseInput input = map parseLine (lines input)

    parseLine :: String -> Step
    parseLine line = Step {
        direction = directionEnum,
        distance = read (head (drop 1 lineParts)),
        stepColour = Colour { colour = colourString }
    } where
        directionEnum = (case directionChar of
            'U' -> U
            'D' -> D
            'L' -> L
            'R' -> R)
        directionChar = head (head lineParts)
        colourString = take 6 (drop 2 (head (drop 2 lineParts)))
        lineParts = splitOn " " (trim line)

    initialField :: Field
    initialField = Field { dugCells = [], currentX = 0, currentY = 0 }

    processStep :: Field -> Step -> Field
    processStep field step = head (drop (distance step) (iterate (\f -> digTrench f (direction step) (stepColour step)) field))

    digTrench :: Field -> Direction -> Colour -> Field
    digTrench field direction col = Field {
        dugCells = (dugCells field) ++ [newDugCell],
        currentX = newX,
        currentY = newY
    } where
        newX = case direction of
            L -> ((currentX field) - 1)
            R -> ((currentX field) + 1)
            _ -> (currentX field)
        newY = case direction of
            U -> ((currentY field) - 1)
            D -> ((currentY field) + 1)
            _ -> (currentY field)
        newDugCell = DugCell { xPos = newX, yPos = newY, cellColour = col }

    findBoundingRectangle :: Field -> ((Int, Int), (Int, Int))
    findBoundingRectangle field = ((minY, minX), (maxY, maxX)) where
        cells = map (\c -> (yPos c, xPos c)) (dugCells field)
        maxY = maximum (map fst cells)
        minY = minimum (map fst cells)
        maxX = maximum (map snd cells)
        minX = minimum (map snd cells)

    isInsideShape :: [(Int, Int)] -> (Int, Int) -> Int -> Bool
    isInsideShape cells (y, x) distToCheck = or [not (cellEmpty cells (y, x)), insideShape] where
        insideShape = True

    cellEmpty :: [(Int, Int)] -> (Int, Int) -> Bool
    cellEmpty cells (y, x) = length (filter (\c -> and [fst c == y, snd c == x]) cells) == 0