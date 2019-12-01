package uk.co.cemerson.aoc.Day1

import uk.co.cemerson.aoc.AOCDay

class Day1 : AOCDay {
    override fun part1() {
        val totalMass = readFileLines(filename = "Day1/input.txt")
                .map(String::toInt)
                .map { calculateFuelBasedOnMass(it) }
                .reduce { a, b -> a + b }

        println("Total mass is " + totalMass)
    }

    override fun part2() {
        val totalMass = readFileLines(filename = "Day1/input.txt")
                .map(String::toInt)
                .map { calculateFuelBasedOnMassInclFuel(it) }
                .reduce { a, b -> a + b }

        println("Total mass is " + totalMass)
    }

    private fun calculateFuelBasedOnMass(mass: Int): Int = mass / 3 - 2

    private fun calculateFuelBasedOnMassInclFuel(mass: Int): Int =
            generateSequence(calculateFuelBasedOnMass(mass)) { calculateFuelBasedOnMass(it) }
                    .takeWhile { it > 0 }
                    .reduce { a, b -> a + b }
}
