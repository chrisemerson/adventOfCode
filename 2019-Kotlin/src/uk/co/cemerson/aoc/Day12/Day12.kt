package uk.co.cemerson.aoc.Day12

import uk.co.cemerson.aoc.AOCDay
import java.lang.Math.abs

class Day12 : AOCDay {
    override fun part1() =
            println(
                    generateSequence(step(getMoonsFromInput())) { step(it) }
                            .take(1000)
                            .map { getTotalEnergy(it) }
                            .last())

    override fun part2() =
            println(
                    "Repeats after "
                            + listOf('x', 'y', 'z')
                            .map { findCycleLengthforAxis(it) }
                            .reduce { a, b -> lcm(a, b) }
                            + " steps")

    private fun findCycleLengthforAxis(axis: Char): Long {
        val initialState = getHashFromMoonState(getMoonsFromInput(), axis)

        return generateSequence(getMoonsFromInput()) { step(it) }
                .map { getHashFromMoonState(it, axis) }
                .drop(1)
                .takeWhile { it != initialState }
                .count() + 1
                .toLong()
    }

    private fun step(moons: List<Moon>): List<Moon> =
            moons
                    .map { calculateVelocities(it, moons) }
                    .map { calculateNewPosition(it) }

    private fun calculateVelocities(moon: Moon, otherMoons: List<Moon>): Moon {
        var returnMoon = moon

        otherMoons.forEach {
            returnMoon = calculateVelocity(returnMoon, it)
        }

        return returnMoon
    }

    private fun calculateVelocity(moon: Moon, otherMoon: Moon): Moon {
        var vx = 0
        var vy = 0
        var vz = 0

        if (moon.x > otherMoon.x) {
            vx -= 1
        } else if (moon.x < otherMoon.x) {
            vx += 1
        }

        if (moon.y > otherMoon.y) {
            vy -= 1
        } else if (moon.y < otherMoon.y) {
            vy += 1
        }

        if (moon.z > otherMoon.z) {
            vz -= 1
        } else if (moon.z < otherMoon.z) {
            vz += 1
        }

        return Moon(moon.x, moon.y, moon.z, moon.vx + vx, moon.vy + vy, moon.vz + vz)
    }

    private fun calculateNewPosition(moon: Moon): Moon =
            Moon(moon.x + moon.vx, moon.y + moon.vy, moon.z + moon.vz, moon.vx, moon.vy, moon.vz)

    private fun getTotalEnergy(moons: List<Moon>): Int = moons
            .map { (abs(it.x) + abs(it.y) + abs(it.z)) * (abs(it.vx) + abs(it.vy) + abs(it.vz)) }
            .reduce { a, b -> a + b }

    private fun getHashFromMoonState(moons: List<Moon>, axis: Char): String =
            when (axis) {
                'x' -> moons
                        .map { it.x.toString() + "|" + it.vx.toString() }
                        .joinToString("||")

                'y' -> moons
                        .map { it.y.toString() + "|" + it.vy.toString() }
                        .joinToString("||")

                'z' -> moons
                        .map { it.z.toString() + "|" + it.vz.toString() }
                        .joinToString("||")

                else -> "?"
            }

    private fun lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

    private fun gcd(a: Long, b: Long): Long = if (b == 0L) a else gcd(b, a % b)

    private fun getMoonsFromInput(): List<Moon> =
            readFileLines("Day12/input.txt")
                    .map {
                        val matches = Regex("\\<x=(-?\\d+),\\sy=(-?\\d+),\\sz=(-?\\d+)\\>").matchEntire(it);
                        Moon(
                                matches!!.groups[1]!!.value.toInt(),
                                matches.groups[2]!!.value.toInt(),
                                matches.groups[3]!!.value.toInt(),
                                0,
                                0,
                                0
                        )
                    }
}