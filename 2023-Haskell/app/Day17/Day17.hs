module Day17.Day17 where
    import Data.List (minimumBy)
    import Data.Vector (toList)
    import Grid
    import Util

    data NodeInfo = NodeInfo { shortestDist :: Int, shortestPath :: [Coord], visited :: Bool } deriving (Show, Eq)

    instance Ord NodeInfo where
        compare a b = compare (shortestDist a) (shortestDist b)

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ getShortestDistToTarget parsedInput (0, 0) (12, 12) where
        parsedInput = parseInput input

    part2 input = show $ parsedInput where
        parsedInput = parseInput input

    parseInput input = convertToIntGrid input

    getShortestDistToTarget :: Grid Int -> Coord -> Coord -> Grid NodeInfo
    getShortestDistToTarget grid startNode targetNode = nodeInfoGrid where
        nodeInfoGrid = dijkstra grid startNode startNodeInfoGrid
        startNodeInfoGrid = changeGridCell (fillGrid (height grid) (width grid) otherNodeInfo) startNode startNodeInfo
        startNodeInfo = NodeInfo { shortestDist = 0, shortestPath = [startNode], visited = True }
        otherNodeInfo = NodeInfo { shortestDist = 999999, shortestPath = [], visited = False }

    dijkstra :: Grid Int -> Coord -> Grid NodeInfo -> Grid NodeInfo
    dijkstra grid startNode nodeInfoGrid = if sum (gmap (\_ r -> glength (gfilter (\_ c -> not (visited c)) r)) nodeInfoGrid) <= 1
        then nodeInfoGrid
        else dijkstra grid nextNodeToVisit newMinNodeInfoGrid where
            newMinNodeInfoGrid = changeGridCell minNodeInfoGrid nextNodeToVisit newNodeInfo
            newNodeInfo = NodeInfo { shortestDist = shortestDist currentNodeInfo, shortestPath = shortestPath currentNodeInfo, visited = True }
            currentNodeInfo = getGridCell minNodeInfoGrid nextNodeToVisit
            nextNodeToVisit = head (filter (\n -> and [not (visited (getGridCell nodeInfoGrid n)), (shortestDist (getGridCell minNodeInfoGrid n)) == lowestUnvisitedDist]) allNodes)
            allNodes = [(y, x) | y <- range 0 ((height grid) - 1), x <- range 0 ((width grid) - 1)]
            lowestUnvisitedDist = shortestDist (minimum (gmap (\_ r -> minimum (gfilter (\_ c -> not (visited c)) r)) nodeInfoGrid))
            minNodeInfoGrid = foldl getMinNodeInfoGrid nodeInfoGrid infoGridsPerAdjacentNode
            infoGridsPerAdjacentNode = map (\n -> (changeGridCell nodeInfoGrid n NodeInfo {
                shortestDist = (shortestDist (getGridCell nodeInfoGrid startNode)) + (getGridCell grid n),
                shortestPath = pathSoFar ++ [n],
                visited = visited (getGridCell nodeInfoGrid n) })) adjacentNodes
            adjacentNodes = findPossibleMoves grid pathSoFar
            pathSoFar = shortestPath (getGridCell nodeInfoGrid startNode)

    findPossibleMoves :: Grid Int -> [Coord] -> [Coord]
    findPossibleMoves grid pathSoFar = possibleMovesNotVisitingSameSquares where
        possibleMovesNotVisitingSameSquares = filter (\pm -> not (elem pm pathSoFar)) possibleMovesWithinRules
        possibleMovesWithinRules = filter (\pm -> or [not (mustTurn last4Spots), pm /= forwardMove]) possibleMovesInsideGrid
        forwardMove = if (length last2Spots) < 2 then (-1, -1) else (2 * (fst (last last2Spots)) - fst (head last2Spots), (2 * (snd (last last2Spots))) - snd (head last2Spots))
        lastSpot = last last2Spots
        last2Spots = if lengthOfPathSoFar >= 2 then drop (lengthOfPathSoFar - 2) pathSoFar else [(-1, -1)]
        last4Spots = drop (lengthOfPathSoFar - 4) pathSoFar
        lengthOfPathSoFar = length pathSoFar
        possibleMovesInsideGrid = filter (\pm -> isInsideGrid grid pm) possibleMoves
        possibleMoves = map (\pm -> (fst pm + fst currentSpot, snd pm + snd currentSpot)) possibleSteps
        possibleSteps = [(0, 1), (0, -1), (1, 0), (-1, 0)]
        currentSpot = last pathSoFar

    getMinNodeInfoGrid :: Grid NodeInfo -> Grid NodeInfo -> Grid NodeInfo
    getMinNodeInfoGrid gridA gridB = gmap (\y r -> gmap (\x c -> min (getGridCell gridB (y, x)) c) r) gridA

    mustTurn :: [Coord] -> Bool
    mustTurn last4Points = if length last4Points < 4 then False else or [
        and [snd (head last4Points) == snd (head (drop 1 last4Points)), snd (head (drop 1 last4Points)) == snd (head (drop 2 last4Points)), snd (head (drop 2 last4Points)) == snd (head (drop 3 last4Points))],
        and [fst (head last4Points) == fst (head (drop 1 last4Points)), fst (head (drop 1 last4Points)) == fst (head (drop 2 last4Points)), fst (head (drop 2 last4Points)) == fst (head (drop 3 last4Points))]]

    isInsideGrid :: Grid a -> Coord -> Bool
    isInsideGrid grid (y, x) = and [y >= 0, y < height grid, x >= 0, x < width grid]
