package uk.co.cemerson.aoc.Day2

import uk.co.cemerson.aoc.AOCDay

class Day2 : AOCDay {
    override fun part1() {
        val program = getProgram().toMutableList()
        val programResult = executeProgram(program.take(1) + listOf(12) + listOf(2) + program.drop(3))

        println("Opcode at position 0 is: " + programResult)
    }

    override fun part2() {
        val program = getProgram().toMutableList()

        for (noun in 0..99) {
            for (verb in 0..99) {
                if (executeProgram(program.take(1) + listOf(noun) + listOf(verb) + program.drop(3)) == 19690720) {
                    println("Noun: " + noun + ", Verb: " + verb)
                    println("Answer: " + (100 * noun + verb))
                }
            }
        }
    }

    private fun getProgram(): List<Int> = readFileSplitByChar(filename = "Day2/input.txt", splitBy = ',')
            .map(String::toInt)

    private fun executeProgram(program: List<Int>): Int =
            generateSequence(Triple(0, program, false)) { executeProgramStep(it) }
                    .takeWhile { !it.third }
                    .map { it.second[0] }
                    .last()

    private fun executeProgramStep(programState: Triple<Int, List<Int>, Boolean>): Triple<Int, List<Int>, Boolean> =
            if (programState.second[programState.first] == 1) performAddStep(programState)
            else if (programState.second[programState.first] == 2) performMultiplyStep(programState)
            else Triple(programState.first, programState.second, true)

    private fun performAddStep(programState: Triple<Int, List<Int>, Boolean>): Triple<Int, List<Int>, Boolean> = Triple(
            programState.first + 4,
            programState.second.take(programState.second[programState.first + 3])
                    + listOf(programState.second[programState.second[programState.first + 1]] + programState.second[programState.second[programState.first + 2]])
                    + programState.second.drop(programState.second[programState.first + 3] + 1),
            false
    )

    private fun performMultiplyStep(programState: Triple<Int, List<Int>, Boolean>): Triple<Int, List<Int>, Boolean> = Triple(
            programState.first + 4,
            programState.second.take(programState.second[programState.first + 3])
                    + listOf(programState.second[programState.second[programState.first + 1]] * programState.second[programState.second[programState.first + 2]])
                    + programState.second.drop(programState.second[programState.first + 3] + 1),
            false
    )
}
