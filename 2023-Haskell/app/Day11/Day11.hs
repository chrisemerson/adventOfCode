module Day11.Day11 where
    import Data.Vector (fromList, ifilter, Vector, imap, toList, head, ifoldl)
    import Grid
    import Util

    data GalaxyPair = GalaxyPair { galaxyA :: Galaxy, galaxyB :: Galaxy } deriving (Show)
    data Galaxy = Galaxy { row :: Int, col :: Int } deriving (Show, Eq)

    instance Eq GalaxyPair where
        x == y = or [and [galaxyA x == galaxyB y, galaxyB x == galaxyA y], and [galaxyA x == galaxyA y, galaxyB x == galaxyB y]]

    vhead = Data.Vector.head

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ getSumOfGalaxyDistances galaxyPairs expansionRows expansionCols 2 where
        galaxyPairs = rmdups (getGalaxyPairs galaxies)
        expansionRows = getExpansionRows universe
        expansionCols = getExpansionColumns universe
        galaxies = findGalaxies universe
        universe = convertToGrid input

    part2 input = show $ getSumOfGalaxyDistances galaxyPairs expansionRows expansionCols 1000000 where
        galaxyPairs = rmdups (getGalaxyPairs galaxies)
        expansionRows = getExpansionRows universe
        expansionCols = getExpansionColumns universe
        galaxies = findGalaxies universe
        universe = convertToGrid input

    getGalaxyPairs galaxies = [GalaxyPair { galaxyA = x, galaxyB = y } | x <- galaxies, y <- galaxies, x /= y]

    findGalaxies universe = concat $ imap findGalaxiesInRow universe

    findGalaxiesInRow rowNo universeRow =
        ifoldl (\a i x -> if x == '#' then a ++ [Galaxy { row = rowNo, col = i }] else a) [] universeRow

    getExpansionRows :: Vector (Vector Char) -> [Int]
    getExpansionRows universe = toList (ifilter (\i x -> not (rowHasGalaxy universe x)) (imap (\i x -> i) universe))

    getExpansionColumns :: Vector (Vector Char) -> [Int]
    getExpansionColumns universe = toList (ifilter (\i x -> not (columnHasGalaxy universe x)) (imap (\i x -> i) (vhead universe)))

    rowHasGalaxy :: Vector (Vector Char) -> Int -> Bool
    rowHasGalaxy universe rowNo = length (ifilter (\i x -> x == '#') (getByIndex universe rowNo)) > 0

    columnHasGalaxy :: Vector (Vector Char) -> Int -> Bool
    columnHasGalaxy universe colNo = length (ifilter (\i x -> (getByIndex x colNo) == '#') universe) > 0

    getSumOfGalaxyDistances :: [GalaxyPair] -> [Int] -> [Int] -> Int -> Int
    getSumOfGalaxyDistances galaxyPairs expansionRows expansionCols expansionAmount =
     sum (map (\x -> getDistanceBetweenGalaxies (galaxyA x) (galaxyB x) expansionRows expansionCols expansionAmount) galaxyPairs)

    getDistanceBetweenGalaxies :: Galaxy -> Galaxy -> [Int] -> [Int] -> Int -> Int
    getDistanceBetweenGalaxies a b expansionRows expansionCols expansionAmount =
        rowsCrossed + colsCrossed + totalExpansionRowIncrease + totalExpansionColIncrease where
            rowsCrossed = abs ((row b) - (row a))
            colsCrossed = abs ((col b) - (col a))
            totalExpansionRowIncrease = expansionScale * (length expansionRowsCrossed)
            totalExpansionColIncrease = expansionScale * (length expansionColsCrossed)
            expansionRowsCrossed = filter (\x -> and [x > (min (row a) (row b)), x < (max (row a) (row b))]) expansionRows
            expansionColsCrossed = filter (\x -> and [x > (min (col a) (col b)), x < (max (col a) (col b))]) expansionCols
            expansionScale = expansionAmount - 1
