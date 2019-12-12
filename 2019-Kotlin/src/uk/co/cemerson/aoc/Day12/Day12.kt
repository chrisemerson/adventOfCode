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

    override fun part2() {
        var counts = mutableMapOf<Char, Int>()

        for (axis in 'x'..'z') {
            counts[axis] = 1

            var currentState = getMoonsFromInput()
            val initialState = getHashFromMoonState(currentState, axis)

            currentState = step(currentState)

            while (getHashFromMoonState(currentState, axis) != initialState) {
                currentState = step(currentState)
                counts[axis] = counts[axis]!! + 1
            }
        }

        val answer = counts
                .map { it.value.toLong() }
                .reduce { a, b -> lcm(a, b) }

        println("Repeats after " + answer + " steps")
    }

    private fun step(moons: List<Moon>): List<Moon> =
            calculateVelocities(moons)
                    .map { calculateNewPosition(it) }

    private fun calculateVelocities(moons: List<Moon>): List<Moon> {
        var moon1 = moons.get(0)
        var moon2 = moons.get(1)
        var moon3 = moons.get(2)
        var moon4 = moons.get(3)

        val moonpair12 = calculateVelocity(Pair(moon1, moon2))
        moon1 = moonpair12.first
        moon2 = moonpair12.second

        val moonpair13 = calculateVelocity(Pair(moon1, moon3))
        moon1 = moonpair13.first
        moon3 = moonpair13.second

        val moonpair14 = calculateVelocity(Pair(moon1, moon4))
        moon1 = moonpair14.first
        moon4 = moonpair14.second

        val moonpair23 = calculateVelocity(Pair(moon2, moon3))
        moon2 = moonpair23.first
        moon3 = moonpair23.second

        val moonpair24 = calculateVelocity(Pair(moon2, moon4))
        moon2 = moonpair24.first
        moon4 = moonpair24.second

        val moonpair34 = calculateVelocity(Pair(moon3, moon4))
        moon3 = moonpair34.first
        moon4 = moonpair34.second

        return listOf(moon1, moon2, moon3, moon4);
    }

    private fun calculateVelocity(moons: Pair<Moon, Moon>): Pair<Moon, Moon> {
        var moon1vx = 0
        var moon1vy = 0
        var moon1vz = 0
        var moon2vx = 0
        var moon2vy = 0
        var moon2vz = 0

        if (moons.first.x > moons.second.x) {
            moon1vx -= 1
            moon2vx += 1
        } else if (moons.first.x < moons.second.x) {
            moon1vx += 1
            moon2vx -= 1
        }

        if (moons.first.y > moons.second.y) {
            moon1vy -= 1
            moon2vy += 1
        } else if (moons.first.y < moons.second.y) {
            moon1vy += 1
            moon2vy -= 1
        }

        if (moons.first.z > moons.second.z) {
            moon1vz -= 1
            moon2vz += 1
        } else if (moons.first.z < moons.second.z) {
            moon1vz += 1
            moon2vz -= 1
        }

        return Pair(
                Moon(moons.first.x, moons.first.y, moons.first.z, moons.first.vx + moon1vx, moons.first.vy + moon1vy, moons.first.vz + moon1vz),
                Moon(moons.second.x, moons.second.y, moons.second.z, moons.second.vx + moon2vx, moons.second.vy + moon2vy, moons.second.vz + moon2vz)
        )

    }

    private fun calculateNewPosition(moon: Moon): Moon =
            Moon(
                    moon.x + moon.vx,
                    moon.y + moon.vy,
                    moon.z + moon.vz,
                    moon.vx,
                    moon.vy,
                    moon.vz
            )

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