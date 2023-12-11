module Grid where
    import Data.Vector (fromList, head, ifilter, Vector)
    import Util

    convertToGrid :: String -> Vector (Vector Char)
    convertToGrid string = fromList (map (\x -> fromList (trim x)) (lines string))

    getGridChar :: Vector (Vector Char) -> Int -> Int -> Char
    getGridChar grid y x = getByIndex (getByIndex grid y) x

    getByIndex :: Vector a -> Int -> a
    getByIndex vector idx = Data.Vector.head (ifilter (\i x -> i == idx) vector)
