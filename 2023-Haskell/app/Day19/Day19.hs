module Day19.Day19 where
    import Data.List.Split
    import Util

    part1 :: String -> String
    part2 :: String -> String

    data Status = Accepted | Rejected deriving (Show, Eq)
    data Condition = Lt | Gt | Al deriving (Show, Eq)
    data Filter = Filter { attribute :: Char, condition :: Condition, value :: Int, result :: String } deriving (Show)
    data Workflow = Workflow { name :: String, filters :: [Filter] } deriving (Show)
    data Part = Part { x :: Int, m :: Int, a :: Int, s :: Int } deriving (Show)
    data ParsedInput = ParsedInput { workflows :: [Workflow], parts :: [Part] } deriving (Show)

    part1 input = show $ sum (map (\p -> (x p) + (m p) + (a p) + (s p)) acceptedParts) where
        acceptedParts = filter (\p -> partAccepted inputWorkflows p) inputParts
        inputWorkflows = (workflows parsedInput)
        inputParts = (parts parsedInput)
        parsedInput = parseInput input

    part2 input = show $ workflows (parseInput input)

    parseInput :: String -> ParsedInput
    parseInput input = ParsedInput { workflows = workflows, parts = parts } where
        workflows = parseWorkflows (head inputParts)
        parts = parseParts (last inputParts)
        inputParts = splitOn "\n\n" input

    parseWorkflows :: String -> [Workflow]
    parseWorkflows workflowsString = map parseWorkflow (lines workflowsString)

    parseWorkflow :: String -> Workflow
    parseWorkflow workflowLine = Workflow { name = workflowName, filters = workflowFilters } where
        workflowName = take ((length workflowLine) - (length (dropWhile (/= '{') workflowLine))) workflowLine
        workflowFiltersString = drop ((length workflowName) + 1) workflowLine
        workflowFilters = map parseFilter (splitOn "," (take ((length workflowFiltersString) - 1) workflowFiltersString))

    parseFilter :: String -> Filter
    parseFilter filterString = if (not (stringContains filterString ":"))
        then Filter { attribute = '_', condition = Al, value = 0, result = filterString }
        else Filter { attribute = thisAttribute, condition = thisCondition, value = thisValue, result = last filterParts } where
            thisCondition = (case head (drop 1 (head filterParts)) of
                '>' -> Gt
                '<' -> Lt)
            thisAttribute = head $ head conditionParts
            thisValue = read $ last conditionParts
            conditionParts = concat $ map (\s -> splitOn ">" s) (splitOn "<" (head filterParts))
            filterParts = splitOn ":" filterString

    parseParts :: String -> [Part]
    parseParts partsString = map parsePart (lines partsString)

    parsePart :: String -> Part
    parsePart partLine = Part { x = x, m = m, a = a, s = s } where
        x = read (last (head (filter (\a -> head a == "x") partAttributes)))
        m = read (last (head (filter (\a -> head a == "m") partAttributes)))
        a = read (last (head (filter (\a -> head a == "a") partAttributes)))
        s = read (last (head (filter (\a -> head a == "s") partAttributes)))
        partAttributes = (map (\p -> splitOn "=" p) (splitOn "," partParts))
        partParts = take ((length partLine) - 2) (drop 1 (partLine))

    partAccepted :: [Workflow] -> Part -> Bool
    partAccepted thisWorkflows thisPart = (putPartThroughWorkflowByName thisPart thisWorkflows "in") == Accepted

    putPartThroughWorkflowByName :: Part -> [Workflow] -> String -> Status
    putPartThroughWorkflowByName thisPart thisWorkflows workflowName = workflowResult where
        workflowResult = putPartThroughFilters thisPart (filters thisWorkflow) thisWorkflows
        thisWorkflow = getWorkflow thisWorkflows workflowName

    putPartThroughFilters :: Part -> [Filter] -> [Workflow] -> Status
    putPartThroughFilters thisPart thisFilters thisWorkflows = if passesTestFilter
        then (case (result testFilter) of
            "A" -> Accepted
            "R" -> Rejected
            _ -> putPartThroughWorkflowByName thisPart thisWorkflows (result testFilter))
        else (putPartThroughFilters thisPart (drop 1 thisFilters) thisWorkflows) where
        passesTestFilter = partPassesFilter testFilter thisPart
        testFilter = head thisFilters

    partPassesFilter thisFilter thisPart = passes where
        passes = (case (condition thisFilter) of
            Al -> True
            Gt -> (actual > testValue)
            Lt -> (actual < testValue))
        actual = (case (attribute thisFilter) of
            'x' -> (x thisPart)
            'm' -> (m thisPart)
            'a' -> (a thisPart)
            's' -> (s thisPart))
        testValue = (value thisFilter)

    getWorkflow :: [Workflow] -> String -> Workflow
    getWorkflow thisWorkflows searchName = head (filter (\w -> (name w) == searchName) thisWorkflows)
