package uk.co.cemerson.aoc.Day18

import uk.co.cemerson.aoc.AOCDay

class Day18 : AOCDay {
    private val pathCache = mutableMapOf<Pair<Map<Pair<Int, Int>, Char>, Map<Char, Pair<Pair<Int, Int>, Int>>>, Pair<String, Int>>()
    private val pathCachePt2 = mutableMapOf<Pair<Map<Pair<Int, Int>, Char>, Map<Char, Pair<Pair<Int, Int>, Pair<Pair<Int, Int>, Int>>>>, Pair<String, Int>>()

    override fun part1() {
        val grid = inputGrid()

        val shortestPath = findShortestPaths(grid, findAvailableKeys(grid))

        println(shortestPath)
    }

    override fun part2() {
        val grid = inputGridPart2()

        val shortestPath = findShortestPathsPt2(grid, findAllAvailableKeys(grid))

        println(shortestPath)
    }

    private fun findShortestPathsPt2(grid: Map<Pair<Int, Int>, Char>, availableKeys: Map<Char, Pair<Pair<Int, Int>, Pair<Pair<Int, Int>, Int>>>): Pair<String, Int>? {
        if (pathCachePt2.containsKey(Pair(grid, availableKeys))) {
            return pathCachePt2[Pair(grid, availableKeys)]
        }

        if (availableKeys.count() == 0) {
            //We're done!
            return Pair("", 0)
        }

        val paths = mutableListOf<Pair<String, Int>>()

        //Try all available keys recursively, pick the shortest
        availableKeys.forEach {
            val thisKey = it.key
            val robotThatCanSeeKey = it.value.second.first
            val unlockedGrid = unlockDoorWithSpecifiedRobot(grid, thisKey, robotThatCanSeeKey)
            val distanceToThisKey = it.value.second.second

            val shortestPaths = findShortestPathsPt2(unlockedGrid, findAllAvailableKeys(unlockedGrid))

            if (shortestPaths != null) {
                paths.add(Pair(thisKey.toString() + shortestPaths.first, shortestPaths.second + distanceToThisKey))
            }
        }

        val shortestPath = paths.minBy { it.second }

        pathCachePt2[Pair(grid, availableKeys)] = shortestPath!!

        return shortestPath
    }

    private fun findAllAvailableKeys(grid: Map<Pair<Int, Int>, Char>): Map<Char, Pair<Pair<Int, Int>, Pair<Pair<Int, Int>, Int>>> {
        val availableKeys = mutableMapOf<Char, Pair<Pair<Int, Int>, Pair<Pair<Int, Int>, Int>>>()

        grid.filter { it.value == '@' }.forEach {
            val thisLoc = it.key

            availableKeys += findAvailableKeys(grid
                    .mapValues { if (it.value == '@') '.' else it.value }
                    .mapValues { if (it.key == thisLoc) '@' else it.value })
                    .mapValues { Pair(it.value.first, Pair(thisLoc, it.value.second)) }
        }

        return availableKeys
    }

    private fun findShortestPaths(grid: Map<Pair<Int, Int>, Char>, availableKeys: Map<Char, Pair<Pair<Int, Int>, Int>>): Pair<String, Int>? {
        if (pathCache.containsKey(Pair(grid, availableKeys))) {
            return pathCache[Pair(grid, availableKeys)]
        }

        if (availableKeys.count() == 0) {
            //We're done!
            return Pair("", 0)
        }

        val paths = mutableListOf<Pair<String, Int>>()

        //Try all available keys recursively, pick the shortest
        availableKeys.forEach {
            val thisKey = it.key
            val unlockedGrid = unlockDoor(grid, thisKey)
            val distanceToThisKey = it.value.second

            val shortestPaths = findShortestPaths(unlockedGrid, findAvailableKeys(unlockedGrid))

            if (shortestPaths != null) {
                paths.add(Pair(thisKey.toString() + shortestPaths.first, shortestPaths.second + distanceToThisKey))
            }
        }

        val shortestPath = paths.minBy { it.second }

        pathCache.put(Pair(grid, availableKeys), shortestPath!!)

        return shortestPath
    }

    private fun unlockDoor(grid: Map<Pair<Int, Int>, Char>, doorToUnlock: Char): Map<Pair<Int, Int>, Char> =
            unlockDoorWithSpecifiedRobot(grid, doorToUnlock, grid.filter { it.value == '@' }.map { it.key }.first())

    private fun unlockDoorWithSpecifiedRobot(grid: Map<Pair<Int, Int>, Char>, doorToUnlock: Char, robotToUnlockDoor: Pair<Int, Int>): Map<Pair<Int, Int>, Char> {
        val keyPosition = grid.filter { it.value == doorToUnlock.toLowerCase() }.map { it.key }.first()
        val doorPosition = grid.filter { it.value == doorToUnlock.toUpperCase() }.map { it.key }.firstOrNull()

        val newGrid = grid.toMutableMap()

        newGrid[robotToUnlockDoor] = '.'
        newGrid[keyPosition] = '@'

        if (doorPosition != null) {
            newGrid[doorPosition] = '.'
        }

        return newGrid
    }

    private fun findAvailableKeys(grid: Map<Pair<Int, Int>, Char>): Map<Char, Pair<Pair<Int, Int>, Int>> {
        val availableKeys = mutableMapOf<Char, Pair<Pair<Int, Int>, Int>>()

        var previousVisited = mutableMapOf<Pair<Int, Int>, Int>()
        var visited = mutableMapOf<Pair<Int, Int>, Int>()

        while (previousVisited != visited || visited.count() == 0) {
            previousVisited = visited
            visited = applyDistances(grid, previousVisited).toMutableMap()
        }

        val maxX = grid.map { it.key.first }.max()!!
        val maxY = grid.map { it.key.second }.max()!!

        for (y in 0..maxY) {
            for (x in 0..maxX) {
                if (grid[Pair(x, y)] in 'a'..'z') {
                    //Is there a distance marker next to us? Find the lowest

                    val distancesToKey = listOf(
                            visited[Pair(x + 1, y)],
                            visited[Pair(x - 1, y)],
                            visited[Pair(x, y + 1)],
                            visited[Pair(x, y - 1)]
                    )
                            .filterNotNull()

                    if (distancesToKey.count() != 0) {
                        val minDistanceToKey = distancesToKey
                                .map { it }
                                .min()!!

                        availableKeys[grid[Pair(x, y)]!!] = Pair(Pair(x, y), minDistanceToKey + 1)
                    }
                }
            }
        }

        return availableKeys
    }

    private fun applyDistances(grid: Map<Pair<Int, Int>, Char>, visited: Map<Pair<Int, Int>, Int>): Map<Pair<Int, Int>, Int> {
        val visitedOut = visited.toMutableMap()

        if (visited.count() == 0) {
            val currentPosition = grid.filter { it.value == '@' }.map { it.key }.first()

            visitedOut[Pair(currentPosition.first, currentPosition.second)] = 0

            if (grid[Pair(currentPosition.first + 1, currentPosition.second)] == '.') visitedOut[Pair(currentPosition.first + 1, currentPosition.second)] = 1
            if (grid[Pair(currentPosition.first - 1, currentPosition.second)] == '.') visitedOut[Pair(currentPosition.first - 1, currentPosition.second)] = 1
            if (grid[Pair(currentPosition.first, currentPosition.second + 1)] == '.') visitedOut[Pair(currentPosition.first, currentPosition.second + 1)] = 1
            if (grid[Pair(currentPosition.first, currentPosition.second - 1)] == '.') visitedOut[Pair(currentPosition.first, currentPosition.second - 1)] = 1
        } else {
            val maxVisited = visited.map { it.value }.max()!!

            visited.filter { it.value == maxVisited }.forEach {
                if (grid[Pair(it.key.first + 1, it.key.second)] == '.' && !visited.containsKey(Pair(it.key.first + 1, it.key.second))) visitedOut[Pair(it.key.first + 1, it.key.second)] = maxVisited + 1
                if (grid[Pair(it.key.first - 1, it.key.second)] == '.' && !visited.containsKey(Pair(it.key.first - 1, it.key.second))) visitedOut[Pair(it.key.first - 1, it.key.second)] = maxVisited + 1
                if (grid[Pair(it.key.first, it.key.second + 1)] == '.' && !visited.containsKey(Pair(it.key.first, it.key.second + 1))) visitedOut[Pair(it.key.first, it.key.second + 1)] = maxVisited + 1
                if (grid[Pair(it.key.first, it.key.second - 1)] == '.' && !visited.containsKey(Pair(it.key.first, it.key.second - 1))) visitedOut[Pair(it.key.first, it.key.second - 1)] = maxVisited + 1
            }
        }

        return visitedOut
    }

    private fun inputGrid(): Map<Pair<Int, Int>, Char> {
        val input = getInput()
        val inputGrid = mutableMapOf<Pair<Int, Int>, Char>()

        var x = 0
        var y = 0

        input.forEach {
            it.forEach {
                inputGrid[Pair(x, y)] = it
                x++
            }

            x = 0
            y++
        }

        return inputGrid
    }

    private fun inputGridPart2(): Map<Pair<Int, Int>, Char> {
        val inputGrid = inputGrid().toMutableMap()
        val currentPosition = inputGrid.filter { it.value == '@' }.map { it.key }.first()

        inputGrid[Pair(currentPosition.first - 1, currentPosition.second - 1)] = '@'
        inputGrid[Pair(currentPosition.first, currentPosition.second - 1)] = '#'
        inputGrid[Pair(currentPosition.first + 1, currentPosition.second - 1)] = '@'

        inputGrid[Pair(currentPosition.first - 1, currentPosition.second)] = '#'
        inputGrid[Pair(currentPosition.first, currentPosition.second)] = '#'
        inputGrid[Pair(currentPosition.first + 1, currentPosition.second)] = '#'

        inputGrid[Pair(currentPosition.first - 1, currentPosition.second + 1)] = '@'
        inputGrid[Pair(currentPosition.first, currentPosition.second + 1)] = '#'
        inputGrid[Pair(currentPosition.first + 1, currentPosition.second + 1)] = '@'

        return inputGrid
    }

    private fun drawGrid(grid: Map<Pair<Int, Int>, Char>, distances: Map<Pair<Int, Int>, Int>?): Unit {
        val maxX = grid.map { it.key.first }.max()!!
        val maxY = grid.map { it.key.second }.max()!!

        for (y in 0..maxY) {
            for (x in 0..maxX) {
                if (grid[Pair(x, y)] == '#') {
                    print("███")
                } else if (distances != null && distances.containsKey(Pair(x, y))) {
                    print(distances[Pair(x, y)].toString().padStart(3, ' '))
                } else if (grid[Pair(x, y)] == '.') {
                    print("   ")
                } else {
                    print(" " + grid[Pair(x, y)] + " ")
                }
            }

            println()
        }
    }

    private fun getInput(): List<String> = readFileLines("Day18/input.txt")
}