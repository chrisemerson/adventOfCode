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
                .map { runSettingsThroughAmplifiersInLoop(it) }
                .max()

        println("Max thrust: " + result)
    }

    private fun getProgram(): List<Int> =
            readFileSplitByChar(filename = "Day7/input.txt", splitBy = ',')
                    .map(String::toInt)

    private fun runSettingsThroughAmplifiers(it: String): Int {
        val program = getProgram()

        val outputA = OutputCollector()
        val outputB = OutputCollector()
        val outputC = OutputCollector()
        val outputD = OutputCollector()
        val outputE = OutputCollector()

        val computerA = Computer(SpecifiedInputProvider(listOf(it[0].toString().toInt(), 0)), outputA)
        computerA.execute(program)

        val computerB = Computer(SpecifiedInputProvider(listOf(it[1].toString().toInt(), outputA.getOutput()[0])), outputB)
        computerB.execute(program)

        val computerC = Computer(SpecifiedInputProvider(listOf(it[2].toString().toInt(), outputB.getOutput()[0])), outputC)
        computerC.execute(program)

        val computerD = Computer(SpecifiedInputProvider(listOf(it[3].toString().toInt(), outputC.getOutput()[0])), outputD)
        computerD.execute(program)

        val computerE = Computer(SpecifiedInputProvider(listOf(it[4].toString().toInt(), outputD.getOutput()[0])), outputE)
        computerE.execute(program)

        return outputE.getOutput()[0]
    }

    private fun runSettingsThroughAmplifiersInLoop(it: String): Int {
        val program = getProgram()

        val ABLink = AmplifierLink(listOf(it[0].toString().toInt()))
        val BCLink = AmplifierLink(listOf(it[1].toString().toInt()))
        val CDLink = AmplifierLink(listOf(it[2].toString().toInt()))
        val DELink = AmplifierLink(listOf(it[3].toString().toInt()))
        val EALink = AmplifierLink(listOf(it[4].toString().toInt(), 0))

        val computerA = Computer(EALink, ABLink)
        val computerB = Computer(ABLink, BCLink)
        val computerC = Computer(BCLink, CDLink)
        val computerD = Computer(CDLink, DELink)
        val computerE = Computer(DELink, EALink)

        var programStateA = Triple(0, program, false)
        var programStateB = Triple(0, program, false)
        var programStateC = Triple(0, program, false)
        var programStateD = Triple(0, program, false)
        var programStateE = Triple(0, program, false)

        while (!programStateA.third || !programStateB.third || !programStateC.third || !programStateD.third || !programStateE.third) {
            programStateA = computerA.executeProgramStep(programStateA)
            programStateB = computerB.executeProgramStep(programStateB)
            programStateC = computerC.executeProgramStep(programStateC)
            programStateD = computerD.executeProgramStep(programStateD)
            programStateE = computerE.executeProgramStep(programStateE)
        }

        return EALink.getOutput().first()
    }
}