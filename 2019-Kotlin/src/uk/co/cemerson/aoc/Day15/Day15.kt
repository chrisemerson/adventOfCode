package uk.co.cemerson.aoc.Day15

import uk.co.cemerson.aoc.AOCDay
import uk.co.cemerson.aoc.Util.IntCode.Computer
import java.math.BigInteger

class Day15 : AOCDay {
    override fun part1() {
        val program = getProgram()
        val repairDroid = RepairDroid(false)
        val computer = Computer(repairDroid, repairDroid)

        computer.execute(program)

        val maze = repairDroid.getMaze()
        val distancesFromHome = repairDroid.getDistancesFromHome()
        val oxygenSystemLocation = repairDroid.getOxygenSystemLocation()

        drawMaze(maze, oxygenSystemLocation, distancesFromHome)

        println()
        println("Shortest path to oxygen system: " + distancesFromHome[Pair(oxygenSystemLocation.first, oxygenSystemLocation.second)])
    }

    override fun part2() {
        val program = getProgram()
        val repairDroid = RepairDroid(true)
        val computer = Computer(repairDroid, repairDroid)

        computer.execute(program)

        val maze = repairDroid.getMaze()
        val oxygenSystemLocation = repairDroid.getOxygenSystemLocation()

        val distancesFromOxygenSystem = repairDroid.getDistancesFromHome()

        drawMaze(maze, oxygenSystemLocation, distancesFromOxygenSystem)

        println()
        println("Time taken to fill with oxygen: " + distancesFromOxygenSystem.map { it.value }.max() + " minutes")
    }

    private fun drawMaze(maze: Map<Pair<Int, Int>, Boolean>, oxygenSystemLocation: Pair<Int, Int>, distancesFromHome: Map<Pair<Int, Int>, Int>) {
        val minX = maze.map { it.key.first }.min()!!
        val maxX = maze.map { it.key.first }.max()!!
        val minY = maze.map { it.key.second }.min()!!
        val maxY = maze.map { it.key.second }.max()!!

        for (y in maxY.downTo(minY)) {
            for (x in minX..maxX) {
                if (x == 0 && y == 0) {
                    print(" o ")
                } else if (x == oxygenSystemLocation.first && y == oxygenSystemLocation.second) {
                    print(" X ")
                } else if (maze[Pair(x, y)] ?: true) {
                    print("███")
                } else {
                    print(distancesFromHome[Pair(x, y)].toString().padStart(3, ' '))
                }
            }

            println()
        }
    }

    private fun getProgram(): List<BigInteger> =
            readFileSplitByChar(filename = "Day15/input.txt", splitBy = ',')
                    .map { it.toBigInteger() }
}