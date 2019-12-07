package uk.co.cemerson.aoc.Day7

import uk.co.cemerson.aoc.AOCDay
import uk.co.cemerson.aoc.Util.IntCode.Computer
import uk.co.cemerson.aoc.Util.IntCode.OutputCollector
import uk.co.cemerson.aoc.Util.IntCode.SpecifiedInputProvider

class Day7 : AOCDay {
    override fun part1() {
        val result = (0..44444)
                .map { it.toString().padStart(5, '0') }
                .filter { it[0] in '0'..'4' && it[1] in '0'..'4' && it[2] in '0'..'4' && it[3] in '0'..'4' && it[4] in '0'..'4' }
                .filter { it[0] != it[1] && it[0] != it[2] && it[0] != it[3] && it[0] != it[4] }
                .filter { it[1] != it[2] && it[1] != it[3] && it[1] != it[4] }
                .filter { it[2] != it[3] && it[2] != it[4] }
                .filter { it[3] != it[4] }
                .map { runSettingsThroughAmplifiers(it) }
                .max()

        println("Max thrust: " + result)
    }

    override fun part2() {
        val result = (55555..99999)
                .map { it.toString() }
                .filter { it[0] in '5'..'9' && it[1] in '5'..'9' && it[2] in '5'..'9' && it[3] in '5'..'9' && it[4] in '5'..'9' }
                .filter { it[0] != it[1] && it[0] != it[2] && it[0] != it[3] && it[0] != it[4] }
                .filter { it[1] != it[2] && it[1] != it[3] && it[1] != it[4] }
                .filter { it[2] != it[3] && it[2] != it[4] }
                .filter { it[3] != it[4] }
                .map { runSettingsThroughAmplifiers(it) }
                .max()

        println("Max thrust: " + result)    }

    private fun getProgram(): List<Int> =
            readFileSplitByChar(filename = "Day7/input.txt", splitBy = ',')
                    .map(String::toInt)

    private fun runSettingsThroughAmplifiers(it: String): Int {
        val program = getProgram()

        val outputA = OutputCollector()
        val computerA = Computer(SpecifiedInputProvider(listOf(it[0].toString().toInt(), 0)), outputA)
        computerA.execute(program)

        val outputB = OutputCollector()
        val computerB = Computer(SpecifiedInputProvider(listOf(it[1].toString().toInt(), outputA.getOutput()[0])), outputB)
        computerB.execute(program)

        val outputC = OutputCollector()
        val computerC = Computer(SpecifiedInputProvider(listOf(it[2].toString().toInt(), outputB.getOutput()[0])), outputC)
        computerC.execute(program)

        val outputD = OutputCollector()
        val computerD = Computer(SpecifiedInputProvider(listOf(it[3].toString().toInt(), outputC.getOutput()[0])), outputD)
        computerD.execute(program)

        val outputE = OutputCollector()
        val computerE = Computer(SpecifiedInputProvider(listOf(it[4].toString().toInt(), outputD.getOutput()[0])), outputE)
        computerE.execute(program)

        return outputE.getOutput()[0]
    }
}