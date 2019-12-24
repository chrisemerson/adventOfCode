package uk.co.cemerson.aoc.Day24

import uk.co.cemerson.aoc.AOCDay

class Day24 : AOCDay {
    override fun part1() {
        var grid = getInputGrid()
        val gridSequence = mutableListOf<Map<Pair<Int, Int>, Char>>()

        while (grid !in gridSequence) {
            gridSequence.add(grid)
            grid = mutateGridOneStep(grid)
        }

        val firstRepeatedGrid = grid

        println("Biodiversity of first repeated grid: " + calculatebioDiversityRating(firstRepeatedGrid))
    }

    override fun part2() {
        var gridLayers = mutableMapOf(0 to getInputGrid().toMutableMap())
        val minutes = 200

        for (i in 1..minutes) {
            gridLayers = mutateGridOneStepRecursively(gridLayers)
        }

        val numBugs = gridLayers
                .map { it.value.filter { it.key.first != 2 || it.key.second != 2 }.filter { it.value == '#' }.count() }
                .sum()

        println("Number of bugs after " + minutes + " minutes: " + numBugs)
    }

    private fun mutateGridOneStep(grid: Map<Pair<Int, Int>, Char>): Map<Pair<Int, Int>, Char> {
        return grid
                .mapValues {
                    val bugsAdjacentToCurrent = getNumberOfBugsAdjacentToCurrent(grid, it.key.first, it.key.second)

                    if (it.value == '#' && bugsAdjacentToCurrent != 1) {
                        '.'
                    } else if (it.value == '.' && (bugsAdjacentToCurrent == 1 || bugsAdjacentToCurrent == 2)) {
                        '#'
                    } else {
                        it.value
                    }
                }
    }

    private fun getNumberOfBugsAdjacentToCurrent(grid: Map<Pair<Int, Int>, Char>, x: Int, y: Int): Int {
        return grid
                .filter { (it.key.first == x && it.key.second == y - 1) || (it.key.first == x && it.key.second == y + 1) || (it.key.first == x + 1 && it.key.second == y) || (it.key.first == x - 1 && it.key.second == y) }
                .filter { it.value == '#' }
                .count()
    }

    private fun mutateGridOneStepRecursively(gridLayers: MutableMap<Int, MutableMap<Pair<Int, Int>, Char>>): MutableMap<Int, MutableMap<Pair<Int, Int>, Char>> {
        val minLayer = gridLayers.map { it.key }.min()!!
        val maxLayer = gridLayers.map { it.key }.max()!!
        val outputLayers = gridLayers.toMutableMap()

        gridLayers[minLayer - 1] = newLayer()
        gridLayers[maxLayer + 1] = newLayer()

        val newMinMinusOneLayer = newLayer()
        val newMaxPlusOneLayer = newLayer()

        // Add bugs if needed to new outside layer
        if (getNumberOfBugsAdjacentToCurrentRecursively(gridLayers, 2, 1, minLayer - 1) in 1..2) newMinMinusOneLayer[Pair(2, 1)] = '#'
        if (getNumberOfBugsAdjacentToCurrentRecursively(gridLayers, 2, 3, minLayer - 1) in 1..2) newMinMinusOneLayer[Pair(2, 3)] = '#'
        if (getNumberOfBugsAdjacentToCurrentRecursively(gridLayers, 1, 2, minLayer - 1) in 1..2) newMinMinusOneLayer[Pair(1, 2)] = '#'
        if (getNumberOfBugsAdjacentToCurrentRecursively(gridLayers, 3, 2, minLayer - 1) in 1..2) newMinMinusOneLayer[Pair(3, 2)] = '#'

        //Add bugs if needed to new inside layer
        for (x in 0..4) {
            if (getNumberOfBugsAdjacentToCurrentRecursively(gridLayers, x, 0, maxLayer + 1) in 1..2) newMaxPlusOneLayer[Pair(x, 0)] = '#'
            if (getNumberOfBugsAdjacentToCurrentRecursively(gridLayers, x, 4, maxLayer + 1) in 1..2) newMaxPlusOneLayer[Pair(x, 4)] = '#'
        }

        for (y in 0..4) {
            if (getNumberOfBugsAdjacentToCurrentRecursively(gridLayers, 0, y, maxLayer + 1) in 1..2) newMaxPlusOneLayer[Pair(0, y)] = '#'
            if (getNumberOfBugsAdjacentToCurrentRecursively(gridLayers, 4, y, maxLayer + 1) in 1..2) newMaxPlusOneLayer[Pair(4, y)] = '#'
        }

        for (layer in minLayer..maxLayer) {
            outputLayers[layer] = gridLayers[layer]!!
                    .mapValues {
                        if (it.key.first == 2 && it.key.second == 2) it.value

                        val bugsAdjacentToCurrent = getNumberOfBugsAdjacentToCurrentRecursively(gridLayers, it.key.first, it.key.second, layer)

                        if (it.value == '#' && bugsAdjacentToCurrent != 1) {
                            '.'
                        } else if (it.value == '.' && (bugsAdjacentToCurrent == 1 || bugsAdjacentToCurrent == 2)) {
                            '#'
                        } else {
                            it.value
                        }
                    }
                    .toMutableMap()
        }

        outputLayers[maxLayer + 1] = newMaxPlusOneLayer
        outputLayers[minLayer - 1] = newMinMinusOneLayer

        //Remove any empty layers
        val newOutputLayers = outputLayers.filter {
            it.value
                    .filter { it.key.first != 2 || it.key.second != 2 }
                    .filter { it.value == '#' }
                    .count() > 0
        }.toMutableMap()

        //Ensure we haven't skipped any layers between min and max - replace them if we have
        val newMinLayer = newOutputLayers.map { it.key }.min()!!
        val newMaxLayer = newOutputLayers.map { it.key }.max()!!

        for (i in newMinLayer..newMaxLayer) {
            if (!newOutputLayers.containsKey(i)) {
                newOutputLayers[i] = newLayer()
            }
        }

        return newOutputLayers
    }

    private fun newLayer(): MutableMap<Pair<Int, Int>, Char> {
        val newLayer = mutableMapOf<Pair<Int, Int>, Char>()

        for (x in 0..4) {
            for (y in 0..4) {
                newLayer[Pair(x, y)] = '.'
            }
        }

        return newLayer
    }

    private fun getNumberOfBugsAdjacentToCurrentRecursively(gridLayers: Map<Int, Map<Pair<Int, Int>, Char>>, x: Int, y: Int, layer: Int): Int {
        var bugsAdjacent = getNumberOfBugsAdjacentToCurrent(gridLayers[layer]!!.filter { it.key.first != 2 || it.key.second != 2 }, x, y)

        //Consider the middle space if we are adjacent
        if (y == 1 && x == 2) bugsAdjacent += gridLayers[layer + 1]!!.filter { it.key.second == 0 }.filter { it.value == '#' }.count()
        if (y == 2 && x == 1) bugsAdjacent += gridLayers[layer + 1]!!.filter { it.key.first == 0 }.filter { it.value == '#' }.count()
        if (y == 2 && x == 3) bugsAdjacent += gridLayers[layer + 1]!!.filter { it.key.first == 4 }.filter { it.value == '#' }.count()
        if (y == 3 && x == 2) bugsAdjacent += gridLayers[layer + 1]!!.filter { it.key.second == 4 }.filter { it.value == '#' }.count()

        //If we're on the outer edge, consider the space in the layer above too
        if (y == 0) bugsAdjacent += gridLayers[layer - 1]!!.filter { it.key.first == 2 && it.key.second == 1 }.filter { it.value == '#' }.count()
        if (y == 4) bugsAdjacent += gridLayers[layer - 1]!!.filter { it.key.first == 2 && it.key.second == 3 }.filter { it.value == '#' }.count()
        if (x == 0) bugsAdjacent += gridLayers[layer - 1]!!.filter { it.key.first == 1 && it.key.second == 2 }.filter { it.value == '#' }.count()
        if (x == 4) bugsAdjacent += gridLayers[layer - 1]!!.filter { it.key.first == 3 && it.key.second == 2 }.filter { it.value == '#' }.count()

        return bugsAdjacent
    }

    private fun calculatebioDiversityRating(grid: Map<Pair<Int, Int>, Char>): Long {
        var count = 1L
        var bioDiversity = 0L
        val maxX = grid.map { it.key.first }.max()!!
        val maxY = grid.map { it.key.second }.max()!!

        for (y in 0..maxY) {
            for (x in 0..maxX) {
                if (grid[Pair(x, y)] == '#') {
                    bioDiversity += count
                }

                count *= 2
            }
        }

        return bioDiversity
    }

    private fun getInputGrid(): Map<Pair<Int, Int>, Char> {
        val fileContents = readFileLines("Day24/input.txt")
        val grid = mutableMapOf<Pair<Int, Int>, Char>()

        var x = 0
        var y = 0

        fileContents.forEach {
            it.forEach {
                grid[Pair(x, y)] = it
                x++
            }

            x = 0
            y++
        }

        return grid
    }
}