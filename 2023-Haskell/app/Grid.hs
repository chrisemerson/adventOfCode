module Grid where
    import Data.Vector (fromList, head, ifilter, Vector, imap, ifoldl, length, drop, take)
    import Util

    type Grid = Vector (Vector Char)

    gmap = Data.Vector.imap
    gfilter = Data.Vector.ifilter
    gfoldl = Data.Vector.ifoldl
    glength = Data.Vector.length
    ghead = Data.Vector.head
    gdrop = Data.Vector.drop
    gtake = Data.Vector.take

    convertToGrid :: String -> Grid
    convertToGrid string = fromList (map (\x -> fromList (trim x)) (lines string))

    getGridChar :: Grid -> Int -> Int -> Char
    getGridChar grid y x = getByIndex (getByIndex grid y) x

    getByIndex :: Vector a -> Int -> a
    getByIndex vector idx = Data.Vector.head (ifilter (\i x -> i == idx) vector)

    swapGridChars :: Grid -> (Int, Int) -> (Int, Int) -> Grid
    swapGridChars grid from to = newGrid where
        newGrid = gmap (\y r -> gmap (\x c -> if (y, x) == from then originalToChar else if (y, x) == to then originalFromChar else c) r) grid
        originalFromChar = getGridChar grid (fst from) (snd from)
        originalToChar = getGridChar grid (fst to) (snd to)

    height :: Grid -> Int
    height grid = glength grid

    width :: Grid -> Int
    width grid = glength (ghead grid)
