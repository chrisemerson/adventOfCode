module Grid where
    import Data.Vector (fromList, head, ifilter, Vector, imap, ifoldl, length, drop, take, replicate)
    import Util

    type Grid a = Vector (Vector a)
    type Coord = (Int, Int)

    gmap = Data.Vector.imap
    gfilter = Data.Vector.ifilter
    gfoldl = Data.Vector.ifoldl
    glength = Data.Vector.length
    ghead = Data.Vector.head
    gdrop = Data.Vector.drop
    gtake = Data.Vector.take

    convertToGrid :: String -> Grid Char
    convertToGrid string = fromList (map (\x -> fromList (trim x)) (lines string))

    convertToIntGrid :: String -> Grid Int
    convertToIntGrid string = fromList (map (\x -> fromList (map (\xx -> read [xx]) (trim x))) (lines string))

    getGridCell :: Grid a -> Coord -> a
    getGridCell grid (y, x) = getByIndex (getByIndex grid y) x

    getByIndex :: Vector a -> Int -> a
    getByIndex vector idx = Data.Vector.head (ifilter (\i x -> i == idx) vector)

    swapGridCells :: Grid a -> Coord -> Coord -> Grid a
    swapGridCells grid from to = newGrid where
        newGrid = gmap (\y r -> gmap (\x c -> if (y, x) == from then originalToCell else if (y, x) == to then originalFromCell else c) r) grid
        originalFromCell = getGridCell grid from
        originalToCell = getGridCell grid to

    changeGridCell :: Grid a -> Coord -> a -> Grid a
    changeGridCell grid (yPos, xPos) replacement = gmap (\y r -> gmap (\x c -> if (y, x) == (yPos, xPos) then replacement else c) r) grid

    height :: Grid a -> Int
    height grid = glength grid

    width :: Grid a -> Int
    width grid = glength (ghead grid)

    fillGrid :: Int -> Int -> a -> Grid a
    fillGrid height width fillWith = Data.Vector.replicate height (Data.Vector.replicate width fillWith)