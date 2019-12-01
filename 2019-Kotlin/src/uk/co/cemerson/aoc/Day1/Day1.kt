package uk.co.cemerson.aoc.Day1

import uk.co.cemerson.aoc.AOCDay
import java.lang.Math.floor

class Day1 : AOCDay {
    override fun part1() {
        val fileLines = readFileLines(filename = "Day1/input.txt")

        val moduleMasses = fileLines.map(String::toInt)
        val fuelRequired = moduleMasses.map { calculateFuelBasedOnMass(it) }
        val totalMass = fuelRequired.reduce { a, b -> a + b }

        println("Total mass is " + totalMass)
    }

    override fun part2() {
        val fileLines = readFileLines(filename = "Day1/input.txt")

        val moduleMasses = fileLines.map(String::toInt)
        val fuelRequired = moduleMasses.map { calculateFuelBasedOnMassInclFuel(it) }
        val totalMass = fuelRequired.reduce { a, b -> a + b }

        println("Total mass is " + totalMass)
    }

    private fun calculateFuelBasedOnMass(mass: Int): Int = (floor(mass.toDouble() / 3) - 2).toInt()

    private fun calculateFuelBasedOnMassInclFuel(mass: Int): Int =
        if (calculateFuelBasedOnMass(mass) <= 0) 0
        else calculateFuelBasedOnMass(mass) + calculateFuelBasedOnMassInclFuel(calculateFuelBasedOnMass(mass))
}
