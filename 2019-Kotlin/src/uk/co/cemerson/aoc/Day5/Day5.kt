package uk.co.cemerson.aoc.Day5

import uk.co.cemerson.aoc.AOCDay
import uk.co.cemerson.aoc.Util.IntCodeComputer

class Day5 : AOCDay {
    private val computer: IntCodeComputer = IntCodeComputer()

    override fun part1() {
        val program = getProgram()
                .toMutableList()

        computer.execute(program)
    }

    override fun part2() {
        val program = getProgram()
                .toMutableList()

        computer.execute(program)
    }

    private fun getProgram(): List<Int> =
            readFileSplitByChar(filename = "Day5/input.txt", splitBy = ',')
                    .map(String::toInt)
}