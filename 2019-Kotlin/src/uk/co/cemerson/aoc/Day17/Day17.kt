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
        val program = getProgram().toMutableList()

        computer.execute(program)

        ASCII.drawGrid()

        program[0] = 2.toBigInteger()

        val instructionsList = ASCII.getInstructionsList()
        val commonSubSequences = ASCII.findCommonSubsequences(instructionsList)

        println(instructionsList)
//        println(commonSubSequences)

        println(ASCII.splitIntoSubSequences(instructionsList, commonSubSequences))


//        computer.execute(program)
    }

    private fun getProgram(): List<BigInteger> =
            readFileSplitByChar(filename = "Day17/input.txt", splitBy = ',')
                    .map { it.toBigInteger() }
}