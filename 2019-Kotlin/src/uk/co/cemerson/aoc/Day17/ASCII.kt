package uk.co.cemerson.aoc.Day17

import uk.co.cemerson.aoc.Util.IntCode.InputProvider
import uk.co.cemerson.aoc.Util.IntCode.OutputConsumer
import java.math.BigInteger

class ASCII : InputProvider, OutputConsumer {
    private val grid = mutableMapOf<Pair<Int, Int>, Char>()
    private val visited = mutableMapOf<Pair<Int, Int>, Boolean>()

    private var x = 0
    private var y = 0

    private var robotX = 0
    private var robotY = 0
    private var robotDir = 0

    override fun getInput(): BigInteger? {




        return null
    }

    override fun shouldHalt(): Boolean {
        return false
    }

    override fun consumeOutput(output: BigInteger) {
        if (output.toInt().toChar() == '^' || output.toInt().toChar() == '>' || output.toInt().toChar() == 'v' || output.toInt().toChar() == '<') {
            robotX = x
            robotY = y

            robotDir = when (output.toInt().toChar()) {
                '^' -> 0
                '>' -> 1
                'v' -> 2
                '<' -> 3
                else -> 0
            }

            visited[Pair(x, y)] = true
        }

        if (output.toInt() == 10) {
            y++
            x = 0
        } else {
            grid[Pair(x, y)] = output.toInt().toChar()
            x++
        }
    }

    override fun consumeFinalValueInPositionZero(output: BigInteger) = Unit

    fun findIntersectionAlignmentParameters(): Int {
        val maxX = grid.map { it.key.first }.max()!!
        val maxY = grid.map { it.key.second }.max()!!

        val alignmentParameters = mutableListOf<Int>()

        for (y in 1.until(maxY)) {
            for (x in 1.until(maxX)) {
                if (isIntersection(x, y)) {
                    alignmentParameters.add(x * y)
                }
            }
        }

        return alignmentParameters.sum()
    }

    private fun isIntersection(x: Int, y: Int): Boolean =
            grid[Pair(x, y)] == '#' && grid[Pair(x - 1, y)] == '#' && grid[Pair(x, y - 1)] == '#' && grid[Pair(x + 1, y)] == '#' && grid[Pair(x, y + 1)] == '#'

    fun splitIntoSubSequences(list: List<Pair<String, Int>>, subSequences: List<List<Pair<String, Int>>>): List<String> {
        var instructionKey = 0
        val instructionMap = mutableMapOf<Pair<String, Int>, Int>()

        list.groupBy { it }.forEach {
            instructionMap[it.value.first()] = instructionKey++
        }

        val instructionList = list.map { instructionMap[it]!! }

        println(instructionMap)
        println(instructionList)


        return listOf<String>()
    }

    fun findCommonSubsequences(list: List<Pair<String, Int>>): List<List<Pair<String, Int>>> {
        val candidates = mutableListOf<List<Pair<String, Int>>>()

        for (subSeqLengthToTry in 20.downTo(2)) {
            for (start in 0..(list.count() - subSeqLengthToTry)) {
                candidates += list
                        .drop(start)
                        .chunk(subSeqLengthToTry)
                        .groupBy { it }
                        .filter { it.value.count() > 1 }
                        .map { it.key }
            }
        }

        return candidates
    }

    fun getInstructionsList(): List<Pair<String, Int>> {
        val individualMovements = getInstructionsAsIndividualMovements()

        var fCounter = 0
        var outputInstructionsList = mutableListOf<String>()

        for (i in individualMovements.indices) {
            when (individualMovements[i]) {
                'F' -> {
                    fCounter++
                }
                'R', 'L' -> {
                    if (fCounter > 0) {
                        outputInstructionsList.add(fCounter.toString())
                        fCounter = 0
                    }

                    outputInstructionsList.add(individualMovements[i].toString())
                }
            }
        }

        while (outputInstructionsList.last() == "R" || outputInstructionsList.last() == "L") {
            outputInstructionsList = outputInstructionsList.dropLast(1).toMutableList()
        }

        return outputInstructionsList.chunk(2).map { Pair(it.first(), it.drop(1).first().toInt()) }
    }


    private fun getInstructionsAsIndividualMovements(): List<Char> {
        val movements = mutableListOf<Char>()

        while (!visitedEverySpace()) {
            val desiredDirection = shouldChangeDirection()

            if (desiredDirection == null) {
                when (robotDir) {
                    0 -> robotY -= 1
                    1 -> robotX += 1
                    2 -> robotY += 1
                    3 -> robotX -= 1
                }

                visited[Pair(robotX, robotY)] = true
            } else {
                when (desiredDirection) {
                    'L' -> robotDir = (robotDir + 3) % 4
                    'R' -> robotDir = (robotDir + 1) % 4
                }
            }

            movements.add(desiredDirection ?: 'F')
        }

        return movements
    }

    private fun visitedEverySpace(): Boolean = grid.filter { it.value == '#' }.count() == visited.count()

    private fun shouldChangeDirection(): Char? {
        //Are we on an intersection? Keep rovin'...
        if (isIntersection(robotX, robotY)) {
            return null
        }

        //Is the next space in the current direction an intersection we've already visited? Also keep rovin'
        val nextSpaceInCurrentDir = getNextSpace(Pair(robotX, robotY), robotDir)
        if (isIntersection(nextSpaceInCurrentDir.first, nextSpaceInCurrentDir.second)) {
            return null
        }

        //Is the next space in the current direction unvisited scaffold?
        if (grid[Pair(nextSpaceInCurrentDir.first, nextSpaceInCurrentDir.second)] == '#' && visited.filter { it.key.first == nextSpaceInCurrentDir.first && it.key.second == nextSpaceInCurrentDir.second }.count() == 0) {
            return null
        }

        //If not, find unvisited scaffolding around us
        val directionToTravel = when {
            grid[Pair(robotX + 1, robotY)] == '#' && visited.filter { it.key.first == robotX + 1 && it.key.second == robotY }.count() == 0 -> 1
            grid[Pair(robotX, robotY + 1)] == '#' && visited.filter { it.key.first == robotX && it.key.second == robotY + 1 }.count() == 0 -> 2
            grid[Pair(robotX - 1, robotY)] == '#' && visited.filter { it.key.first == robotX - 1 && it.key.second == robotY }.count() == 0 -> 3
            grid[Pair(robotX, robotY - 1)] == '#' && visited.filter { it.key.first == robotX && it.key.second == robotY - 1 }.count() == 0 -> 0
            else -> robotDir
        }

        return if ((robotDir + 1) % 4 == directionToTravel) {
            'R'
        } else if ((robotDir + 3) % 4 == directionToTravel) {
            'L'
        } else {
            null
        }
    }

    private fun getNextSpace(currentSpace: Pair<Int, Int>, currentDir: Int): Pair<Int, Int> =
            when (currentDir) {
                0 -> Pair(currentSpace.first, currentSpace.second - 1)
                1 -> Pair(currentSpace.first + 1, currentSpace.second)
                2 -> Pair(currentSpace.first, currentSpace.second + 1)
                3 -> Pair(currentSpace.first - 1, currentSpace.second)
                else -> currentSpace
            }

    fun drawGrid() {
        val maxX = grid.map { it.key.first }.max()!!
        val maxY = grid.map { it.key.second }.max()!!

        for (y in 0..maxY) {
            for (x in 0..maxX) {
                print(grid[Pair(x, y)])
            }

            println()
        }
    }

    private fun <T> List<T>.chunk(size: Int): List<List<T>> =
            this.withIndex().groupBy { it.index / size }.map { it.value.map { it.value } }
}