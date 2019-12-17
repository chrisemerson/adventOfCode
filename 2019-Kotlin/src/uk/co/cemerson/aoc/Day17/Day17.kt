package uk.co.cemerson.aoc.Day17

import uk.co.cemerson.aoc.AOCDay
import uk.co.cemerson.aoc.Util.IntCode.Computer
import java.math.BigInteger

class Day17 : AOCDay {
    override fun part1() {
        val ASCII = ASCII()
        val computer = Computer(ASCII, ASCII)
        val program = getProgram()

        computer.execute(program)

        ASCII.drawGrid()

        println("Alignment parameter sum: " + ASCII.findIntersectionAlignmentParameters())
    }

    override fun part2() {
        val ASCII = ASCII()
        val computer = Computer(ASCII, ASCII)
        var program = getProgram().toMutableList()

//        computer.execute(program)
//
//        ASCII.drawGrid()

        program = (listOf(2.toBigInteger()) + program.drop(1)).toMutableList()

        computer.execute(program)

        println(ASCII.getStarDustCollected())
    }

    private fun getProgram(): List<BigInteger> =
            readFileSplitByChar(filename = "Day17/input.txt", splitBy = ',')
                    .map { it.toBigInteger() }
}