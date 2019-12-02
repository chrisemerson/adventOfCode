package uk.co.cemerson.aoc.Day2

import uk.co.cemerson.aoc.AOCDay

class Day2 : AOCDay {
    val DESIRED_OUTPUT = 19690720

    override fun part1() {
        val program = getProgram()
                .toMutableList()
                .replace(1, 12)
                .replace(2, 2)

        val programResult = executeProgram(program)

        println("Opcode at position 0 is: " + programResult)
    }

    override fun part2() {
        val program = getProgram().toMutableList()

        for (noun in 0..99) {
            for (verb in 0..99) {
                if (executeProgram(program.replace(1, noun).replace(2, verb)) == DESIRED_OUTPUT) {
                    println("Noun: " + noun + ", Verb: " + verb)
                    println("Answer: " + (100 * noun + verb))
                }
            }
        }
    }

    private fun getProgram(): List<Int> =
            readFileSplitByChar(filename = "Day2/input.txt", splitBy = ',')
                    .map(String::toInt)

    private fun executeProgram(program: List<Int>): Int =
            generateSequence(Triple(0, program, false)) { executeProgramStep(it) }
                    .takeWhile { !it.third }
                    .map { it.second[0] }
                    .last()

    private fun executeProgramStep(programState: Triple<Int, List<Int>, Boolean>): Triple<Int, List<Int>, Boolean> =
            when (programState.second[programState.first]) {
                1 -> performOperation(programState, { a, b -> a + b })
                2 -> performOperation(programState, { a, b -> a * b })
                else -> Triple(programState.first, programState.second, true)
            }

    private fun performOperation(programState: Triple<Int, List<Int>, Boolean>, operation: (Int, Int) -> Int): Triple<Int, List<Int>, Boolean> {
        val (position, program) = programState

        val operand1 = program[program[position + 1]]
        val operand2 = program[program[position + 2]]
        val outputposition = program[position + 3]

        return Triple(position + 4, program.replace(outputposition, operation(operand1, operand2)), false)
    }

    private fun List<Int>.replace(position: Int, newValue: Int) =
            this.take(position) + listOf(newValue) + this.drop(position + 1)
}
