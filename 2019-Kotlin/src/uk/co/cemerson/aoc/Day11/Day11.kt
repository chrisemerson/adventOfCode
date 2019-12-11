package uk.co.cemerson.aoc.Day11

import uk.co.cemerson.aoc.AOCDay
import uk.co.cemerson.aoc.Util.IntCode.Computer
import java.math.BigInteger

class Day11 : AOCDay {
    override fun part1() {
        val input = getProgram()
        val robot = Robot(0.toBigInteger())
        val computer = Computer(robot, robot)

        computer.execute(input)

        println("Panels painted at least once: " + robot.getFinalPanelPaintedCount())
    }

    override fun part2() {
        val input = getProgram()
        val robot = Robot(1.toBigInteger())
        val computer = Computer(robot, robot)

        computer.execute(input)

        robot.outputGrid()
    }

    private fun getProgram(): List<BigInteger> =
            readFileSplitByChar(filename = "Day11/input.txt", splitBy = ',')
                    .map { it.toBigInteger() }
}