module Grid where
    import Data.Vector (fromList, head, ifilter, Vector, imap, ifoldl, length, drop, take, replicate)
    import Util

--    data Grid a = Vector (Vector a)
    type Grid a = Vector (Vector a)

    gmap = Data.Vector.imap
    gfilter = Data.Vector.ifilter
    gfoldl = Data.Vector.ifoldl
    glength = Data.Vector.length
    ghead = Data.Vector.head
    gdrop = Data.Vector.drop
    gtake = Data.Vector.take

    convertToGrid :: String -> Grid Char
    convertToGrid string = fromList (map (\x -> fromList (trim x)) (lines string))

    getGridCell :: Grid a -> Int -> Int -> a
    getGridCell grid y x = getByIndex (getByIndex grid y) x

    getByIndex :: Vector a -> Int -> a
    getByIndex vector idx = Data.Vector.head (ifilter (\i x -> i == idx) vector)

    swapGridChars :: Grid a -> (Int, Int) -> (Int, Int) -> Grid a
    swapGridChars grid from to = newGrid where
        newGrid = gmap (\y r -> gmap (\x c -> if (y, x) == from then originalToChar else if (y, x) == to then originalFromChar else c) r) grid
        originalFromChar = getGridCell grid (fst from) (snd from)
        originalToChar = getGridCell grid (fst to) (snd to)

    height :: Grid a -> Int
    height grid = glength grid

    width :: Grid a -> Int
    width grid = glength (ghead grid)

    fillGrid :: Int -> Int -> a -> Grid a
    fillGrid height width fillWith = Data.Vector.replicate height (Data.Vector.replicate width fillWith)