module Day17.Day17 where
    import Grid

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ findShortestPath parsedInput [(11, 12)] where
        parsedInput = parseInput input

    part2 input = show $ findPossibleMoves parsedInput [(11, 12)] where
        parsedInput = parseInput input

    parseInput input = convertToGrid input

    findShortestPath :: Grid -> [(Int, Int)] -> ([(Int, Int)], Int)
    findShortestPath grid pathSoFar = if and [(fst currentMove) == (height grid - 1), (snd currentMove) == (width grid - 1)]
        then (pathSoFar, heatLossThisSquare)
        else if length possibleMoves == 0
            then ([], 99999)
            else (fst bestPath, (snd bestPath) + heatLossThisSquare) where
                heatLossThisSquare = read [getGridChar grid (fst currentMove) (snd currentMove)]
                currentMove = last pathSoFar
                bestPath = head (filter (\pp -> snd pp == minimumHeatLoss) possiblePaths)
                minimumHeatLoss = minimum (map snd possiblePaths)
                possiblePaths = map (\pm -> findShortestPath grid (pathSoFar ++ [pm])) possibleMoves
                possibleMoves = findPossibleMoves grid pathSoFar

    findPossibleMoves :: Grid -> [(Int, Int)] -> [(Int, Int)]
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

    mustTurn :: [(Int, Int)] -> Bool
    mustTurn last4Points = if length last4Points < 4 then False else or [
        and [snd (head last4Points) == snd (head (drop 1 last4Points)), snd (head (drop 1 last4Points)) == snd (head (drop 2 last4Points)), snd (head (drop 2 last4Points)) == snd (head (drop 3 last4Points))],
        and [fst (head last4Points) == fst (head (drop 1 last4Points)), fst (head (drop 1 last4Points)) == fst (head (drop 2 last4Points)), fst (head (drop 2 last4Points)) == fst (head (drop 3 last4Points))]]

    isInsideGrid grid (y, x) = and [y >= 0, y < height grid, x >= 0, x < width grid]
