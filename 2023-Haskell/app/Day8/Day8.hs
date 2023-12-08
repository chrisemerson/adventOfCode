module Day8.Day8 where
    import Util

    part1 :: String -> String
    part2 :: String -> String

    data Node = Node { nodeId :: String, left :: String, right :: String } deriving (Show)

    part1 input = show $ (length (getPath nodes (concat (repeat instructions)) "AAA") - 1) where
        instructions = fst parsedInput
        nodes = snd parsedInput
        parsedInput = parseInput input

    part2 input = input

    parseInput input = (head inputLines, parseNodes (tail inputLines)) where
        inputLines = filter (\l -> l /= "") (map trim (lines input))

    parseNodes nodeLines = map parseNode nodeLines

    parseNode line = Node { nodeId = take 3 line, left = take 3 (drop 7 line), right = take 3 (drop 12 line) }

    getNode nodes node = head (filter (\n -> (nodeId n) == node) nodes)

    getPath :: [Node] -> [Char] -> String -> [String]
    getPath nodes instructions currentNode = if currentNode == "ZZZ"
        then ["ZZZ"]
        else [currentNode] ++ (getPath nodes (tail instructions) (getNextNode nodes currentNode (head instructions)))

    getNextNode :: [Node] -> String -> Char -> String
    getNextNode nodes currentNode instruction = if instruction == 'L'
        then left (getNode nodes currentNode)
        else right (getNode nodes currentNode)