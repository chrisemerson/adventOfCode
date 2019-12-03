package uk.co.cemerson.aoc.Day3

import uk.co.cemerson.aoc.AOCDay
import java.lang.Math.abs

class Day3 : AOCDay {
    override fun part1() {
        val wires = readFileLines("Day3/input.txt")
                .map { it.trim().split(',') }

        val coords = wires.map { convertWireDefinitionToCoordinates(it) }

        val minDistanceToIntersection = coords[0]
                .intersect(coords[1])
                .map { abs(it.first) + abs(it.second) }
                .filter { it != 0 }
                .min()

        println(minDistanceToIntersection)
    }

    override fun part2() {

    }

    private fun convertWireDefinitionToCoordinates(it: List<String>): MutableList<Pair<Int, Int>> {
        var x = 0
        var y = 0
        var dx = 0
        var dy = 0
        val coords = listOf(Pair(x, y)).toMutableList()

        coords.add(Pair(x, y))

        it.forEach {
            val number = it.drop(1).toInt()

            when (it.first()) {
                'U' -> {
                    dx = 0
                    dy = 1
                }
                'D' -> {
                    dx = 0
                    dy = -1
                }
                'L' -> {
                    dx = -1
                    dy = 0
                }
                'R' -> {
                    dx = 1
                    dy = 0
                }
            }

            for (i in 1..number) {
                x += dx
                y += dy

                coords.add(Pair(x, y))
            }
        }

        return coords
    }
}
