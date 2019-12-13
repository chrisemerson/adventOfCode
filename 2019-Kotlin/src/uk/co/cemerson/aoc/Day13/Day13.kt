package uk.co.cemerson.aoc.Day13

import uk.co.cemerson.aoc.AOCDay
import uk.co.cemerson.aoc.Util.IntCode.Computer
import java.math.BigInteger

class Day13 : AOCDay {
    override fun part1() {
        val arcadeMachine = ArcadeMachine()
        val computer = Computer(arcadeMachine, arcadeMachine)
        val program = getProgram()

        computer.execute(program)

        println(arcadeMachine.getBlockTilesLeft())
    }

    override fun part2() {
        val arcadeMachine = ArcadeMachine()
        val computer = Computer(arcadeMachine, arcadeMachine)
        val program = getProgram().toMutableList()

        program[0] = 2.toBigInteger()

        computer.execute(program)

        println(arcadeMachine.getScore())
    }

    private fun getProgram(): List<BigInteger> =
            readFileSplitByChar(filename = "Day13/input.txt", splitBy = ',')
                    .map { it.toBigInteger() }
}