module Day17.Day17 where
    import Grid
    import Util

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ dijkstra parsedInput (0, 0) (12, 12) distancesGrid unvisitedNodes [] where
        distancesGrid = changeGridCell (fillGrid (height parsedInput) (width parsedInput) 9999) (0, 0) 0
        unvisitedNodes = [(y, x) | y <- range 0 ((height parsedInput) - 1), x <- range 0 ((width parsedInput) - 1), (y, x) /= (0, 0)]
        parsedInput = parseInput input

    part2 input = show $ parsedInput where
        parsedInput = parseInput input

    parseInput input = convertToIntGrid input

    dijkstra :: Grid Int -> Coord -> Coord -> Grid Int -> [Coord] -> [Coord] -> Int
    dijkstra grid startNode targetNode distancesGrid unvisitedNodes pathSoFar = if length unvisitedNodes == 0
        then getGridCell distancesGrid targetNode
        else minPathToHere where
            minPathToHere = 0
            newUnvisitedNodes = filter (\n -> n /= minDistanceNode) unvisitedNodes
            minDistanceNode = filter (\n -> getGridCell minDistanceGrid n == minDistanceToAdjacentNodes) adjacentNodes
            minDistanceToAdjacentNodes = map (\n -> getGridCell minDistanceGrid n) adjacentNodes
            minDistanceGrid = foldl getMinDistanceGrid distanceGrid distanceGridsPerAdjacentNode
            distanceGridsPerAdjacentNode = map (\n -> (changeGridCell distancesGrid n (getGridCell grid n))) adjacentNodes
            adjacentNodes = findPossibleMoves grid (pathSoFar ++ [startNode])

    findPossibleMoves :: Grid Int -> [Coord] -> [Coord]
    findPossibleMoves grid pathSoFar = possibleMovesNotVisitingSameSquares where
        possibleMovesNotVisitingSameSquares = filter (\pm -> not (elem pm pathSoFar)) possibleMovesWithinRules
        possibleMovesWithinRules = filter (\pm -> or [not (mustTurn last4Spots), pm /= forwardMove]) possibleMovesInsideGrid
        forwardMove = if (length last2Spots) < 2 then (-1, -1) else (fst (last last2Spots) - fst (head last2Spots), snd (last last2Spots) - snd (head last2Spots))
        lastSpot = head last2Spots
        last2Spots = if (length pathSoFar) > 1 then take (lengthOfPathSoFar - 2) pathSoFar else [(-1, -1)]
        last4Spots = take (lengthOfPathSoFar - 4) pathSoFar
        lengthOfPathSoFar = length pathSoFar
        possibleMovesInsideGrid = filter (\pm -> isInsideGrid grid pm) possibleMoves
        possibleMoves = map (\pm -> (fst pm + fst currentSpot, snd pm + snd currentSpot)) possibleSteps
        possibleSteps = [(0, 1), (0, -1), (1, 0), (-1, 0)]
        currentSpot = last pathSoFar

    getMinDistanceGrid :: Grid Int -> Grid Int -> Grid Int
    getMinDistanceGrid gridA gridB = gmap (\y r -> gmap (\x c -> min (getGridCell gridB (y, x)) c) r) gridA

    mustTurn :: [Coord] -> Bool
    mustTurn last4Points = if length last4Points < 4 then False else or [
        and [snd (head last4Points) == snd (head (drop 1 last4Points)), snd (head (drop 1 last4Points)) == snd (head (drop 2 last4Points)), snd (head (drop 2 last4Points)) == snd (head (drop 3 last4Points))],
        and [fst (head last4Points) == fst (head (drop 1 last4Points)), fst (head (drop 1 last4Points)) == fst (head (drop 2 last4Points)), fst (head (drop 2 last4Points)) == fst (head (drop 3 last4Points))]]

    isInsideGrid :: Grid a -> Coord -> Bool
    isInsideGrid grid (y, x) = and [y >= 0, y < height grid, x >= 0, x < width grid]
