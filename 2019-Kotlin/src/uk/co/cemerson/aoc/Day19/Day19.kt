package uk.co.cemerson.aoc.Day19

import uk.co.cemerson.aoc.AOCDay
import uk.co.cemerson.aoc.Util.IntCode.Computer
import uk.co.cemerson.aoc.Util.IntCode.OutputCollector
import uk.co.cemerson.aoc.Util.IntCode.SpecifiedInputProvider
import java.math.BigInteger

class Day19 : AOCDay {
    override fun part1() {
        val program = getProgram()
        val droneSystem = DroneSystem(0, 0, 50)
        val computer = Computer(droneSystem, droneSystem)

        for (i in 0.until(50 * 50)) {
            computer.execute(program)
        }

        droneSystem.drawGrid()

        println(droneSystem.getNumberOfAffectedPoints())
    }

    override fun part2() {
        val program = getProgram()

        var x = 100
        var y = 100

        var answerFound = false

        val outputCollector = OutputCollector()

        while (!answerFound) {
            //Try x coord + 100
            val Xcomputer = Computer(SpecifiedInputProvider(listOf(x + 99, y)), outputCollector)
            Xcomputer.execute(program)

            if (outputCollector.getOutput().last().toInt() == 1) {
                //Success! Try y + 100 now
                val Ycomputer = Computer(SpecifiedInputProvider(listOf(x, y + 99)), outputCollector)
                Ycomputer.execute(program)

                if (outputCollector.getOutput().last().toInt() == 1) {
                    //we have our answer
                    answerFound = true
                } else {
                    x++
                }
                //Try y
            } else {
                y++
            }
        }

        println("Result: " + (x * 10000 + y))
    }

    private fun getProgram(): List<BigInteger> =
            readFileSplitByChar(filename = "Day19/input.txt", splitBy = ',')
                    .map { it.toBigInteger() }
}