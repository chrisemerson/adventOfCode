package uk.co.cemerson.aoc.Day10

import uk.co.cemerson.aoc.AOCDay
import java.lang.Math.abs
import java.lang.Math.sqrt

class Day10 : AOCDay {
    override fun part1() {
        val asteroids = getAsteroidsCoords()

        val visibleAsteroidCount = asteroids
                .map { Triple(it.first, it.second, countAsteroidsVisibleFromPoint(it, asteroids)) }

        val maxVisibleAsteroids = visibleAsteroidCount
                .map { it.third }
                .max()

        val bestAsteroid = visibleAsteroidCount
                .filter { it.third == maxVisibleAsteroids }
                .first()

        println("Max visible asteroids from best location: " + maxVisibleAsteroids + " at location (" + bestAsteroid.first + ", " + bestAsteroid.second + ")")
    }

    override fun part2() {
        val asteroidBase = Pair(26, 29)
        val destroyedAsteroids = returnAsteroidDestructionOrder(getAsteroidsCoords(), asteroidBase)

        val asteroid200 = destroyedAsteroids.take(200).last()

        println("Answer: " + (asteroid200.first * 100 + asteroid200.second))
    }

    private fun returnAsteroidDestructionOrder(asteroids: List<Pair<Int, Int>>, base: Pair<Int, Int>): List<Pair<Int, Int>> {
        val distances = asteroids
                .map { Pair(it.first - base.first, it.second - base.second) }
                .filter { it.first != 0 || it.second != 0 }

        val topRightAsteroids = distances
                .filter { it.first >= 0 && it.second < 0 }
                .map { Pair(it, Pair(it.first.toDouble() / it.second.toDouble(), sqrt(((it.first * it.first) + (it.second * it.second)).toDouble()))) }
                .groupBy { it.second.first }
                .toSortedMap(reverseOrder())
                .map { (_, info) -> info.minBy { it.second.second }!! }
                .map { it.first }

        val rightAsteroid = distances
                .filter { it.first > 0 && it.second == 0 }
                .minBy { abs(it.first) }

        val bottomRightAsteroids = distances
                .filter { it.first >= 0 && it.second > 0 }
                .map { Pair(it, Pair(it.first.toDouble() / it.second.toDouble(), sqrt(((it.first * it.first) + (it.second * it.second)).toDouble()))) }
                .groupBy { it.second.first }
                .toSortedMap()
                .map { (_, info) -> info.minBy { it.second.second }!! }
                .map { it.first }

        val bottomLeftAsteroids = distances
                .filter { it.first < 0 && it.second > 0 }
                .map { Pair(it, Pair(it.first.toDouble() / it.second.toDouble(), sqrt(((it.first * it.first) + (it.second * it.second)).toDouble()))) }
                .groupBy { it.second.first }
                .toSortedMap()
                .map { (_, info) -> info.minBy { it.second.second }!! }
                .map { it.first }

        val leftAsteroid = distances
                .filter { it.first < 0 && it.second == 0 }
                .minBy { abs(it.first) }

        val topLeftAsteroids = distances
                .filter { it.first < 0 && it.second < 0 }
                .map { Pair(it, Pair(it.first.toDouble() / it.second.toDouble(), sqrt(((it.first * it.first) + (it.second * it.second)).toDouble()))) }
                .groupBy { it.second.first }
                .toSortedMap(reverseOrder())
                .map { (_, info) -> info.minBy { it.second.second }!! }
                .map { it.first }

        var destroyedAsteroids = topRightAsteroids

        if (rightAsteroid != null) {
            destroyedAsteroids = destroyedAsteroids + rightAsteroid
        }

        destroyedAsteroids = destroyedAsteroids + bottomRightAsteroids + bottomLeftAsteroids

        if (leftAsteroid != null) {
            destroyedAsteroids = destroyedAsteroids + leftAsteroid
        }

        destroyedAsteroids = destroyedAsteroids + topLeftAsteroids

        //Filter out destroyed asteroids and call recursively for completeness

        return destroyedAsteroids.map { Pair(it.first + base.first, it.second + base.second) }
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

    private fun countAsteroidsVisibleFromPoint(point: Pair<Int, Int>, asteroids: MutableList<Pair<Int, Int>>): Int {
        val distances = asteroids
                .map { Pair(it.first - point.first, it.second - point.second) }
                .filter { it.first != 0 || it.second != 0 }

        val sameLineAsteroids = distances
                .filter { it.second == 0 }
                .groupBy { it.first > 0 }
                .count()

        val topAsteroids = distances
                .filter { it.second < 0 }
                .map { Pair(it.first.toDouble() / it.second.toDouble(), sqrt(((it.first * it.first) + (it.second * it.second)).toDouble())) }
                .groupBy { it.first }
                .count()

        val bottomAsteroids = distances
                .filter { it.second > 0 }
                .map { Pair(it.first.toFloat() / it.second.toFloat(), sqrt(((it.first * it.first) + (it.second * it.second)).toDouble())) }
                .groupBy { it.first }
                .count()

        return sameLineAsteroids + topAsteroids + bottomAsteroids;
    }
}