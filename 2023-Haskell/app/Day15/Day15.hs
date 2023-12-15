module Day15.Day15 where
    import Data.Char
    import Data.List.Split
    import Util
    import Data.Vector (Vector, generate, map, (//), take, drop, head)

    vmap = Data.Vector.map
    pmap = Prelude.map
    vtake = Data.Vector.take
    ptake = Prelude.take
    vdrop = Data.Vector.drop
    vhead = Data.Vector.head
    phead = Prelude.head

    data Lens = Lens { label :: String, focalLength :: Int } deriving (Show)
    data Box = Box { number :: Int, lenses :: [Lens] } deriving (Show)

    part1 :: String -> String
    part2 :: String -> String

    part1 input = show $ sum (pmap hash insts) where
        insts = instructions input

    part2 input = show $ findFocusingPower (foldl (\a x -> processInstruction x a) createBoxes insts) where
        insts = instructions input
        boxes = createBoxes

    instructions input = splitOn "," (trim input)

    createBoxes :: Vector Box
    createBoxes = generate 256 (\x -> Box { number = x, lenses = [] })

    processInstruction :: String -> Vector Box -> Vector Box
    processInstruction inst boxes = if stringEndsWith inst "-"
        then processLensRemoval inst boxes
        else processLensInsertion inst boxes

    processLensRemoval :: String -> Vector Box -> Vector Box
    processLensRemoval inst boxes = replaceBox boxes boxNo (removeLensFromBox lensLabel originalBox) where
        lensLabel = (ptake ((length inst) - 1) inst)
        boxNo = hash lensLabel
        originalBox = vhead (vdrop boxNo boxes)

    processLensInsertion :: String -> Vector Box -> Vector Box
    processLensInsertion inst boxes = replaceBox boxes boxNo (insertLensIntoBox lensLabel lensFocalLength originalBox) where
        instructionParts = splitOn "=" inst
        lensLabel = phead instructionParts
        boxNo = hash lensLabel
        originalBox = vhead (vdrop boxNo boxes)
        lensFocalLength = read (last instructionParts) :: Int

    removeLensFromBox :: String -> Box -> Box
    removeLensFromBox lensLabel box = Box {
        number = (number box),
        lenses = filter (\l -> (label l) /= lensLabel) (lenses box)
    }

    insertLensIntoBox :: String -> Int -> Box -> Box
    insertLensIntoBox lensLabel lensFocalLength box = if boxHasLens box lensLabel
        then Box {
            number = (number box),
            lenses = foldl (\a l -> a ++ [if ((label l) == lensLabel)
                                        then Lens { label = lensLabel, focalLength = lensFocalLength }
                                        else l]) [] (lenses box)
        }
        else Box {
            number = (number box),
            lenses = (lenses box) ++ [Lens { label = lensLabel, focalLength = lensFocalLength }]
        }

    boxHasLens :: Box -> String -> Bool
    boxHasLens box lensLabel = length (filter (\l -> (label l) == lensLabel) (lenses box)) == 1

    replaceBox :: Vector Box -> Int -> Box -> Vector Box
    replaceBox boxes boxNo newBox = boxes // [(boxNo, newBox)]

    findFocusingPower :: Vector Box -> Int
    findFocusingPower boxes = sum (vmap findFocusingPowerOfBox boxes)

    findFocusingPowerOfBox :: Box -> Int
    findFocusingPowerOfBox box = fst (foldl (\a x -> ((fst a) + (((number box) + 1) * (snd a) * (focalLength x)), ((snd a) + 1))) (0, 1) (lenses box))

    hash str = (foldl (\a x -> (a + (ord x)) * 17) 0 str) `mod` 256
