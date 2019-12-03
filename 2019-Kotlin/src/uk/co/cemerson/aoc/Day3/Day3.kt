package uk.co.cemerson.aoc.Day3

import uk.co.cemerson.aoc.AOCDay
import java.lang.Math.abs

class Day3 : AOCDay {
    override fun part1() {
        val wires = readFileLines("Day3/input.txt")
                .map { it.trim().split(',') }

        val coords = wires.map { convertWireDefinitionToCoordinates(it) }

        val minDistanceToIntersection = findIntersections(
                coords[0].map { Pair(it.first, it.second) },
                coords[1].map { Pair(it.first, it.second) }
        )
                .map { abs(it.first) + abs(it.second) }
                .filter { it != 0 }
                .min()

        println(minDistanceToIntersection)
    }

    override fun part2() {
        val wires = readFileLines("Day3/input.txt")
                .map { it.trim().split(',') }

        val coords = wires.map { convertWireDefinitionToCoordinates(it) }

        val intersect = findIntersections(
                coords[0].map { Pair(it.first, it.second) },
                coords[1].map { Pair(it.first, it.second) }
        )
                .filter { it.first != 0 || it.second != 0 }

        val intersectionDistances = mutableListOf<Triple<Int, Int, Int>>()

        intersect.forEach {
            val x = it.first
            val y = it.second

            val wire1Distance = coords[0]
                    .filter { it.first == x && it.second == y }
                    .map { it.third }
                    .first()

            val wire2Distance = coords[1]
                    .filter { it.first == x && it.second == y }
                    .map { it.third }
                    .first()

            intersectionDistances.add(Triple(x, y, wire1Distance + wire2Distance))
        }

        val minDistanceByWires = intersectionDistances
                .map { it.third }
                .min()

        println(minDistanceByWires)
    }

    private fun findIntersections(coords1: List<Pair<Int, Int>>, coords2: List<Pair<Int, Int>>): Set<Pair<Int, Int>> =
            coords1.intersect(coords2)

    private fun convertWireDefinitionToCoordinates(it: List<String>): MutableList<Triple<Int, Int, Int>> {
        var x = 0
        var y = 0
        var count = 0
        var dx = 0
        var dy = 0
        val coords = listOf(Triple(x, y, count)).toMutableList()

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
                count++

                coords.add(Triple(x, y, count))
            }
        }

        return coords
    }
}
