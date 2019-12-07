package uk.co.cemerson.aoc.Util.IntCode

class Computer(private val inputProvider: InputProvider, private val outputConsumer: OutputConsumer) {
    fun execute(program: List<Int>): Unit =
            outputConsumer.consumeFinalValueInPositionZero(
                    generateSequence(Triple(0, program, false)) { executeProgramStep(it) }
                            .takeWhile { !it.third }
                            .map { it.second[0] }
                            .last()
            )

    private fun executeProgramStep(programState: Triple<Int, List<Int>, Boolean>): Triple<Int, List<Int>, Boolean> {
        val parameterModes = getParameterModes(programState.second[programState.first])

        return when (programState.second[programState.first] % 100) {
            1 -> performOperation(programState, { a, b -> a + b }, parameterModes)
            2 -> performOperation(programState, { a, b -> a * b }, parameterModes)
            3 -> takeInput(programState)
            4 -> printOutput(programState, parameterModes)
            5 -> jumpIfTrue(programState, parameterModes)
            6 -> jumpIfFalse(programState, parameterModes)
            7 -> lessThan(programState, parameterModes)
            8 -> equals(programState, parameterModes)

            else -> Triple(programState.first, programState.second, true)
        }
    }

    private fun performOperation(programState: Triple<Int, List<Int>, Boolean>, operation: (Int, Int) -> Int, parameterModes: List<Int>): Triple<Int, List<Int>, Boolean> {
        val (position, program) = programState

        val operand1 = getValue(program, position + 1, parameterModes[0])
        val operand2 = getValue(program, position + 2, parameterModes[1])
        val outputposition = program[position + 3]

        return Triple(position + 4, program.replace(outputposition, operation(operand1, operand2)), false)
    }

    private fun takeInput(programState: Triple<Int, List<Int>, Boolean>): Triple<Int, List<Int>, Boolean> {
        val (position, program) = programState

        program.replace(program[position + 1], inputProvider.getInput())

        return Triple(position + 2, program.replace(program[position + 1], inputProvider.getInput()), false)
    }

    private fun printOutput(programState: Triple<Int, List<Int>, Boolean>, parameterModes: List<Int>): Triple<Int, List<Int>, Boolean> {
        val (position, program) = programState
        val output = getValue(program, position + 1, parameterModes[0])

        outputConsumer.consumeOutput(output)

        return Triple(position + 2, program, false)
    }

    private fun jumpIfTrue(programState: Triple<Int, List<Int>, Boolean>, parameterModes: List<Int>): Triple<Int, List<Int>, Boolean> {
        val (position, program) = programState

        return if (getValue(program, position + 1, parameterModes[0]) != 0)
            Triple(getValue(program, position + 2, parameterModes[1]), program, false)
        else Triple(position + 3, program, false)
    }

    private fun jumpIfFalse(programState: Triple<Int, List<Int>, Boolean>, parameterModes: List<Int>): Triple<Int, List<Int>, Boolean> {
        val (position, program) = programState

        return if (getValue(program, position + 1, parameterModes[0]) == 0)
            Triple(getValue(program, position + 2, parameterModes[1]), program, false)
        else Triple(position + 3, program, false)
    }

    private fun lessThan(programState: Triple<Int, List<Int>, Boolean>, parameterModes: List<Int>): Triple<Int, List<Int>, Boolean> {
        val (position, program) = programState

        return if (getValue(program, position + 1, parameterModes[0]) < getValue(program, position + 2, parameterModes[1]))
            Triple(position + 4, program.replace(program[position + 3], 1), false)
        else Triple(position + 4, program.replace(program[position + 3], 0), false)
    }

    private fun equals(programState: Triple<Int, List<Int>, Boolean>, parameterModes: List<Int>): Triple<Int, List<Int>, Boolean> {
        val (position, program) = programState

        return if (getValue(program, position + 1, parameterModes[0]) == getValue(program, position + 2, parameterModes[1]))
            Triple(position + 4, program.replace(program[position + 3], 1), false)
        else Triple(position + 4, program.replace(program[position + 3], 0), false)
    }

    private fun getParameterModes(instruction: Int): List<Int> =
            (instruction / 100)
                    .toString()
                    .padStart(3, '0')
                    .toMutableList()
                    .map { it.toString().toInt() }
                    .reversed()

    private fun getValue(program: List<Int>, position: Int, parameterMode: Int): Int =
            if (parameterMode == 0) program[program[position]] else program[position]

    private fun List<Int>.replace(position: Int, newValue: Int) =
            this.take(position) + listOf(newValue) + this.drop(position + 1)
}