package uk.co.cemerson.aoc.Day5

import uk.co.cemerson.aoc.AOCDay
import uk.co.cemerson.aoc.Util.IntCode.Computer
import uk.co.cemerson.aoc.Util.IntCode.PrintLnOutputConsumer
import uk.co.cemerson.aoc.Util.IntCode.SpecifiedInputProvider
import java.math.BigInteger

class Day5 : AOCDay {
    override fun part1() {
        val program = getProgram()
                .toMutableList()

        val computer = Computer(SpecifiedInputProvider(listOf(1)), PrintLnOutputConsumer())

        computer.execute(program)
    }

    override fun part2() {
        val program = getProgram()
                .toMutableList()

        val computer = Computer(SpecifiedInputProvider(listOf(5)), PrintLnOutputConsumer())

        computer.execute(program)
    }

    private fun getProgram(): List<BigInteger> =
            readFileSplitByChar(filename = "Day5/input.txt", splitBy = ',')
                    .map { it.toInt().toBigInteger() }
}