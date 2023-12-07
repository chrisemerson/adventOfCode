module Day7.Day7 where
    import Data.List
    import Data.List.Split
    import Data.Vector (Vector, fromList, ifoldl)
    import Util

    part1 :: String -> String
    part2 :: String -> String

    data Hand = Hand { cards :: String, bid :: Int } deriving (Show, Eq)

    instance Ord Hand where
        compare a b = if handType (scoreHand a) == handType (scoreHand b)
            then compareRankings (ranking (scoreHand a)) (ranking (scoreHand b))
            else compare (handType (scoreHand a)) (handType (scoreHand b))

    data HandType = HighCard | Pair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
        deriving (Enum, Show, Ord, Eq)
    data HandScore = HandScore { handType :: HandType, ranking :: [Int] } deriving (Show)

    part1 input = show $ getWinnings (parseInput input)
    part2 input = input

    getWinnings hands = ifoldl (\a i h -> a + ((bid h) * (i + 1))) 0 sortedHands where
        sortedHands = fromList (sort hands)

    compareRankings a b =
        if and[a == [], b == []]
            then EQ
        else if (head a) == (head b)
            then compareRankings (tail a) (tail b)
        else compare a b

    parseInput input = map (parseHand . trim) (lines input)

    parseHand line = Hand { cards = (head handParts), bid = read (head (tail handParts)) } where
        handParts = splitOn " " line

    scoreHand hand = HandScore { handType = getHandType (cards hand), ranking = map getCardRankingFromCard (cards hand) }

    getHandType cards =
        if hasFiveOfAKind cards
            then FiveOfAKind
        else if hasFourOfAKind cards
            then FourOfAKind
        else if and [hasThreeOfAKind cards, hasPair cards]
            then FullHouse
        else if hasThreeOfAKind cards
            then ThreeOfAKind
        else if hasTwoPair cards
            then TwoPair
        else if hasPair cards
            then Pair
        else
            HighCard

    hasFiveOfAKind cards = length (filter (\f -> snd f == 5) (frequencies cards)) == 1
    hasFourOfAKind cards = length (filter (\f -> snd f == 4) (frequencies cards)) == 1
    hasThreeOfAKind cards = length (filter (\f -> snd f == 3) (frequencies cards)) == 1
    hasTwoPair cards = length (filter (\f -> snd f == 2) (frequencies cards)) == 2
    hasPair cards = length (filter (\f -> snd f == 2) (frequencies cards)) == 1

    getCardRankingFromCard card = case card of
        'A' -> 14
        'K' -> 13
        'Q' -> 12
        'J' -> 11
        'T' -> 10
        _ -> read [card]
