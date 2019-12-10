package uk.co.cemerson.aoc.Day10

import uk.co.cemerson.aoc.AOCDay
import java.lang.Math.abs
import java.lang.Math.sqrt
import java.util.Comparator

class Day10 : AOCDay {
    override fun part1() {
        val visibleAsteroidCount = getListOfAsteroidsWithVisibleCount()

        val maxVisibleAsteroids = visibleAsteroidCount
                .map { it.third }
                .max()

        val bestAsteroid = visibleAsteroidCount
                .filter { it.third == maxVisibleAsteroids }
                .first()

        println("Max visible asteroids from best location: " + maxVisibleAsteroids + " at location (" + bestAsteroid.first + ", " + bestAsteroid.second + ")")
    }

    override fun part2() {
        val asteroidBase =
                getListOfAsteroidsWithVisibleCount()
                        .sortedBy { it.third }
                        .takeLast(1)
                        .map { Pair(it.first, it.second) }
                        .first()

        val asteroid200 =
                getAsteroidDestructionOrder(
                        getAsteroidsCoords(),
                        asteroidBase
                )
                        .take(200)
                        .last()

        println("Answer: " + (asteroid200.first * 100 + asteroid200.second))
    }

    private fun getAsteroidsCoords(): MutableList<Pair<Int, Int>> {
        val input = readFileLines("Day10/input.txt")
        val asteroids = mutableListOf<Pair<Int, Int>>()

        input.forEachIndexed { y, line ->
            line.forEachIndexed { x, cell ->
                if (cell == '#') {
                    asteroids.add(Pair(x, y))
                }
            }
        }

        return asteroids
    }

    private fun getListOfAsteroidsWithVisibleCount(): List<Triple<Int, Int, Int>> {
        val asteroids = getAsteroidsCoords()

        return asteroids
                .map { Triple(it.first, it.second, countAsteroidsVisibleFromPoint(it, asteroids)) }
    }

    private fun countAsteroidsVisibleFromPoint(point: Pair<Int, Int>, asteroids: MutableList<Pair<Int, Int>>): Int {
        val relativeAsteroidPositions = getRelativeAsteroidPositions(asteroids, point)

        return (relativeAsteroidPositions
                .filter { it.second == 0 }
                .groupBy { it.first > 0 }
                .count()
                + countAsteroidsInSectorVisibleFromPoint(relativeAsteroidPositions.filter { it.second < 0 })
                + countAsteroidsInSectorVisibleFromPoint(relativeAsteroidPositions.filter { it.second > 0 }))
    }

    private fun countAsteroidsInSectorVisibleFromPoint(relativeAsteroidPositions: List<Pair<Int, Int>>): Int =
            relativeAsteroidPositions
                    .map { Pair(it.first.toDouble() / it.second.toDouble(), sqrt(((it.first * it.first) + (it.second * it.second)).toDouble())) }
                    .groupBy { it.first }
                    .count()

    private fun getRelativeAsteroidPositions(asteroids: List<Pair<Int, Int>>, base: Pair<Int, Int>): List<Pair<Int, Int>> =
            asteroids
                    .map { Pair(it.first - base.first, it.second - base.second) }
                    .filter { it.first != 0 || it.second != 0 }

    private fun getAsteroidDestructionOrder(asteroids: List<Pair<Int, Int>>, base: Pair<Int, Int>): List<Pair<Int, Int>> {
        if (asteroids.count() <= 1) return listOf()

        val destroyedAsteroids = listOf(
                getAsteroidDestructionOrderForTopRightSector(getRelativeAsteroidPositions(asteroids, base)),
                getAsteroidDestructionOrderForHorizontalRight(getRelativeAsteroidPositions(asteroids, base)),
                getAsteroidDestructionOrderForBottomRightSector(getRelativeAsteroidPositions(asteroids, base)),
                getAsteroidDestructionOrderForBottomLeftSector(getRelativeAsteroidPositions(asteroids, base)),
                getAsteroidDestructionOrderForHorizontalLeft(getRelativeAsteroidPositions(asteroids, base)),
                getAsteroidDestructionOrderForTopLeftSector(getRelativeAsteroidPositions(asteroids, base))
        )
                .flatten()
                .map { Pair(it.first + base.first, it.second + base.second) }

        return (destroyedAsteroids
                + getAsteroidDestructionOrder(
                asteroids.filter { it !in destroyedAsteroids },
                base
        ))
    }

    private fun getAsteroidDestructionOrderForTopRightSector(relativeAsteroidPositions: List<Pair<Int, Int>>): List<Pair<Int, Int>> =
            relativeAsteroidPositions
                    .filter { it.first >= 0 && it.second < 0 }
                    .getAsteroidDestructionOrderForSector(reverseOrder())

    private fun getAsteroidDestructionOrderForHorizontalRight(relativeAsteroidPositions: List<Pair<Int, Int>>): List<Pair<Int, Int>> =
            getAsteroidDestructionOrderOnHorizontalLine(
                    relativeAsteroidPositions
                            .filter { it.first > 0 && it.second == 0 }
            )

    private fun getAsteroidDestructionOrderForBottomRightSector(relativeAsteroidPositions: List<Pair<Int, Int>>): List<Pair<Int, Int>> =
            relativeAsteroidPositions
                    .filter { it.first >= 0 && it.second > 0 }
                    .getAsteroidDestructionOrderForSector(naturalOrder())

    private fun getAsteroidDestructionOrderForBottomLeftSector(relativeAsteroidPositions: List<Pair<Int, Int>>): List<Pair<Int, Int>> =
            relativeAsteroidPositions
                    .filter { it.first < 0 && it.second > 0 }
                    .getAsteroidDestructionOrderForSector(naturalOrder())

    private fun getAsteroidDestructionOrderForHorizontalLeft(relativeAsteroidPositions: List<Pair<Int, Int>>): List<Pair<Int, Int>> =
            getAsteroidDestructionOrderOnHorizontalLine(
                    relativeAsteroidPositions
                            .filter { it.first < 0 && it.second == 0 }
            )

    private fun getAsteroidDestructionOrderForTopLeftSector(relativeAsteroidPositions: List<Pair<Int, Int>>): List<Pair<Int, Int>> =
            relativeAsteroidPositions
                    .filter { it.first < 0 && it.second < 0 }
                    .getAsteroidDestructionOrderForSector(reverseOrder())

    private fun getAsteroidDestructionOrderOnHorizontalLine(asteroidsOnHorizontalLine: List<Pair<Int, Int>>): List<Pair<Int, Int>> =
            listOf(asteroidsOnHorizontalLine.minBy { abs(it.first) })
                    .filterNotNull()

    private fun List<Pair<Int, Int>>.getAsteroidDestructionOrderForSector(comparator: Comparator<Double>): List<Pair<Int, Int>> =
            this
                    .map { Pair(it, Pair(it.first.toDouble() / it.second.toDouble(), sqrt(((it.first * it.first) + (it.second * it.second)).toDouble()))) }
                    .groupBy { it.second.first }
                    .toSortedMap(comparator)
                    .map { (_, info) -> info.minBy { it.second.second }!! }
                    .map { it.first }
}