package uk.co.cemerson.aoc.Day2

import uk.co.cemerson.aoc.AOCDay

class Day2 : AOCDay {
    override fun part1() {
        var program = getProgram().toMutableList()

        program[1] = 12
        program[2] = 2

        val programResult = executeProgram(program)

        println("Opcode at position 0 is: " + programResult)
    }

    override fun part2() {
        var program = getProgram().toMutableList()

        for (noun in 0..99) {
            for (verb in 0..99) {
                program[1] = noun
                program[2] = verb

                if (executeProgram(program) == 19690720) {
                    println("Noun: " + noun + ", Verb: " + verb)
                    println("Answer: " + (100 * noun + verb))
                }
            }
        }
    }

    private fun getProgram(): List<Int> {
        return readFileSplitByChar(filename = "Day2/input.txt", splitBy = ',')
                .map(String::toInt)
    }

    private fun executeProgram(program: List<Int>): Int {
        var opCodes = program.toMutableList()
        var position = 0

        while (opCodes[position] != 99) {
            if (opCodes[position] == 1) {
                opCodes.set(opCodes[position + 3], opCodes[opCodes[position + 1]] + opCodes[opCodes[position + 2]])
                position += 4
            } else if (opCodes[position] == 2) {
                opCodes.set(opCodes[position + 3], opCodes[opCodes[position + 1]] * opCodes[opCodes[position + 2]])
                position += 4
            }
        }

        return opCodes[0]
    }
}
