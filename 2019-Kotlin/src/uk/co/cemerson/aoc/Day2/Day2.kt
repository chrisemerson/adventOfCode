package uk.co.cemerson.aoc.Day2

import uk.co.cemerson.aoc.AOCDay
import uk.co.cemerson.aoc.Util.IntCode.Computer
import uk.co.cemerson.aoc.Util.IntCode.ZeroInputProvider
import java.math.BigInteger

class Day2 : AOCDay {
    private val DESIRED_OUTPUT = 19690720
    private val outputConsumer = GetFinalProgramStateOutputConsumer()
    private val computer = Computer(ZeroInputProvider(), outputConsumer)

    override fun part1() {
        val program = getProgram()
                .toMutableList()
                .replace(1, 12.toBigInteger())
                .replace(2, 2.toBigInteger())

        computer.execute(program)
        val programResult = outputConsumer.getFinalOutput()

        println("Opcode at position 0 is: " + programResult)
    }

    override fun part2() {
        val program = getProgram().toMutableList()

        for (noun in 0..99) {
            for (verb in 0..99) {
                computer.execute(program.replace(1, noun.toBigInteger()).replace(2, verb.toBigInteger()))

                if (outputConsumer.getFinalOutput().toInt() == DESIRED_OUTPUT) {
                    println("Noun: " + noun + ", Verb: " + verb)
                    println("Answer: " + (100 * noun + verb))
                }
            }
        }
    }

    private fun getProgram(): List<BigInteger> =
            readFileSplitByChar(filename = "Day2/input.txt", splitBy = ',')
                    .map { it.toInt().toBigInteger() }

    private fun List<BigInteger>.replace(position: Int, newValue: BigInteger) =
            this.take(position) + listOf(newValue) + this.drop(position + 1)
}
