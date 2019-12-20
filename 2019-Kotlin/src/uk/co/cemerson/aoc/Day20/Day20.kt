package uk.co.cemerson.aoc.Day20

import uk.co.cemerson.aoc.AOCDay

class Day20 : AOCDay {
    override fun part1() {
        val grid = inputGrid()

        val portals = findPortals(grid)

        var visited = mutableMapOf<Pair<Int, Int>, Int>()
        var previousVisited = mutableMapOf<Pair<Int, Int>, Int>()

        visited[portals.filter { it.second == "AA" }.map { it.first }.first()] = 0

        while (previousVisited != visited) {
            previousVisited = visited
            visited = applyDistances(grid, previousVisited, portals).toMutableMap()
        }

        drawGrid(grid, visited)

        println("Moves to end of maze: " + visited[portals.filter { it.second == "ZZ" }.map { it.first }.first()])
    }

    override fun part2() {
        val grid = inputGrid()
        val portals = findPortals(grid)

        var visited = mutableMapOf<Int, MutableMap<Pair<Int, Int>, Int>>()

        visited[0] = mutableMapOf()
        visited[0]!![portals.filter { it.second == "AA" }.map { it.first }.first()] = 0

        visited = visitMazes(grid, portals, visited, 0)



        println(visited[0])
    }

    private fun visitMazes(
            grid: Map<Pair<Int, Int>, Char>,
            portals: List<Pair<Pair<Int, Int>, String>>,
            visited: MutableMap<Int, MutableMap<Pair<Int, Int>, Int>>,
            level: Int
    ): MutableMap<Int, MutableMap<Pair<Int, Int>, Int>> {
        var previousVisited = mutableMapOf<Pair<Int, Int>, Int>()
        val visitedOut = visited

        while (previousVisited != visitedOut) {
            previousVisited = visitedOut[level]!!
            visitedOut[level] = applyDistances(grid, previousVisited, portals).toMutableMap()
        }

        return visitedOut
    }

    private fun applyDistances(grid: Map<Pair<Int, Int>, Char>, visited: Map<Pair<Int, Int>, Int>, portals: List<Pair<Pair<Int, Int>, String>>): Map<Pair<Int, Int>, Int> {
        val visitedOut = visited.toMutableMap()

        val maxVisited = visited.map { it.value }.max()!!

        visited.filter { it.value == maxVisited }.forEach {
            if (grid[Pair(it.key.first + 1, it.key.second)] == '.' && !visited.containsKey(Pair(it.key.first + 1, it.key.second))) visitedOut[Pair(it.key.first + 1, it.key.second)] = maxVisited + 1
            if (grid[Pair(it.key.first - 1, it.key.second)] == '.' && !visited.containsKey(Pair(it.key.first - 1, it.key.second))) visitedOut[Pair(it.key.first - 1, it.key.second)] = maxVisited + 1
            if (grid[Pair(it.key.first, it.key.second + 1)] == '.' && !visited.containsKey(Pair(it.key.first, it.key.second + 1))) visitedOut[Pair(it.key.first, it.key.second + 1)] = maxVisited + 1
            if (grid[Pair(it.key.first, it.key.second - 1)] == '.' && !visited.containsKey(Pair(it.key.first, it.key.second - 1))) visitedOut[Pair(it.key.first, it.key.second - 1)] = maxVisited + 1
        }

        val portalsOnThisSpace = portals.filter { visitedOut[Pair(it.first.first, it.first.second)] == maxVisited }

        if (portalsOnThisSpace.count() > 0) {
            val portalName = portalsOnThisSpace.map { it.second }.first()

            //Find other portal exit
            val portalExit = portals.filter { !visitedOut.containsKey(it.first) && it.second == portalName }

            if (portalExit.count() != 0) {
                val portalExitCoords = portalExit.map { it.first }.first()
                visitedOut[portalExitCoords] = maxVisited + 1
            }
        }

        return visitedOut
    }

    private fun findPortals(grid: Map<Pair<Int, Int>, Char>): List<Pair<Pair<Int, Int>, String>> {
        val portals = mutableListOf<Pair<Pair<Int, Int>, String>>()

        val maxX = grid.map { it.key.first }.max()!!
        val maxY = grid.map { it.key.second }.max()!!

        for (y in 0..maxY) {
            for (x in 0..maxX) {
                if (portals.filter { it.first.first == x && it.first.second == y }.count() == 0 && grid[Pair(x, y)] in 'A'..'Z') {
                    //Check space below and to right
                    if (grid[Pair(x + 1, y)] in 'A'..'Z') {
                        val portalName = grid[Pair(x, y)].toString() + grid[Pair(x + 1, y)].toString()

                        if (grid[Pair(x + 2, y)] == '.') {
                            portals.add(Pair(Pair(x + 2, y), portalName))
                        } else if (grid[Pair(x - 1, y)] == '.') {
                            portals.add(Pair(Pair(x - 1, y), portalName))
                        }
                    } else if (grid[Pair(x, y + 1)] in 'A'..'Z') {
                        val portalName = grid[Pair(x, y)].toString() + grid[Pair(x, y + 1)].toString()

                        if (grid[Pair(x, y + 2)] == '.') {
                            portals.add(Pair(Pair(x, y + 2), portalName))
                        } else if (grid[Pair(x, y - 1)] == '.') {
                            portals.add(Pair(Pair(x, y - 1), portalName))
                        }
                    }
                }
            }
        }

        return portals
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

    private fun drawGrid(grid: Map<Pair<Int, Int>, Char>, distances: Map<Pair<Int, Int>, Int>?): Unit {
        val maxX = grid.map { it.key.first }.max()!!
        val maxY = grid.map { it.key.second }.max()!!

        for (y in 0..maxY) {
            for (x in 0..maxX) {
                if (!grid.containsKey(Pair(x, y))) {
                    print("   ")
                } else if (grid[Pair(x, y)] == '#') {
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

    private fun getInput(): List<String> = readFileLines("Day20/input.txt")
}