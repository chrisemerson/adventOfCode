import qualified Day1.Day1 as Day1
import qualified Day2.Day2 as Day2
import qualified Day3.Day3 as Day3
import qualified Day4.Day4 as Day4
import qualified Day5.Day5 as Day5
import qualified Day6.Day6 as Day6
import qualified Day7.Day7 as Day7
import qualified Day8.Day8 as Day8
import qualified Day9.Day9 as Day9
import qualified Day10.Day10 as Day10
import qualified Day11.Day11 as Day11
import qualified Day12.Day12 as Day12
import qualified Day13.Day13 as Day13
import qualified Day14.Day14 as Day14
import qualified Day15.Day15 as Day15
import qualified Day16.Day16 as Day16
import qualified Day17.Day17 as Day17
import qualified Day18.Day18 as Day18
import qualified Day19.Day19 as Day19
import qualified Day20.Day20 as Day20
import qualified Day21.Day21 as Day21
import qualified Day22.Day22 as Day22
import qualified Day23.Day23 as Day23
import qualified Day24.Day24 as Day24
import qualified Day25.Day25 as Day25
import System.Environment

main = do
   args <- getArgs
   case args of
       [dayNo, partNo] -> runAOC (read dayNo) (read partNo)
       _               -> error $ "You must pass a Day number and a Part number on the command line"

runAOC :: Int -> Int -> IO ()

runAOC dayNo partNo = do
    if or [dayNo > 25, dayNo < 1]
        then error $ "Invalid day number: " ++ show dayNo
        else do
            input <- readFile ("Day" ++ show dayNo ++ "/input.txt")
            runDayAndPart dayNo partNo input

runDayAndPart :: Int -> Int -> String -> IO()

runDayAndPart dayNo partNo input = putStrLn result where
    [part1Fn, part2Fn] = case dayNo of
        1  -> [Day1.part1, Day1.part2]
        2  -> [Day2.part1, Day2.part2]
        3  -> [Day3.part1, Day3.part2]
        4  -> [Day4.part1, Day4.part2]
        5  -> [Day5.part1, Day5.part2]
        6  -> [Day6.part1, Day6.part2]
        7  -> [Day7.part1, Day7.part2]
        8  -> [Day8.part1, Day8.part2]
        9  -> [Day9.part1, Day9.part2]
        10 -> [Day10.part1, Day10.part2]
        11 -> [Day11.part1, Day11.part2]
        12 -> [Day12.part1, Day12.part2]
        13 -> [Day13.part1, Day13.part2]
        14 -> [Day14.part1, Day14.part2]
        15 -> [Day15.part1, Day15.part2]
        16 -> [Day16.part1, Day16.part2]
        17 -> [Day17.part1, Day17.part2]
        18 -> [Day18.part1, Day18.part2]
        19 -> [Day19.part1, Day19.part2]
        20 -> [Day20.part1, Day20.part2]
        21 -> [Day21.part1, Day21.part2]
        22 -> [Day22.part1, Day22.part2]
        23 -> [Day23.part1, Day23.part2]
        24 -> [Day24.part1, Day24.part2]
        25 -> [Day25.part1, Day25.part2]
    result = case partNo of
        1 -> part1Fn input
        2 -> part2Fn input
        _ -> error $ "Invalid part number: " ++ show partNo
