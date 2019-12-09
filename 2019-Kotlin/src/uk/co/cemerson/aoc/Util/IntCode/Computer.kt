package uk.co.cemerson.aoc.Util.IntCode

import java.math.BigInteger

class Computer(private val inputProvider: InputProvider, private val outputConsumer: OutputConsumer) {
    var relativeBase = 0.toBigInteger()

    fun execute(program: List<BigInteger>): Unit =
            outputConsumer.consumeFinalValueInPositionZero(
                    generateSequence(Triple(0, program, false)) { executeProgramStep(it) }
                            .takeWhile { !it.third }
                            .map { it.second[0] }
                            .last()
            )

    fun executeProgramStep(programState: Triple<Int, List<BigInteger>, Boolean>): Triple<Int, List<BigInteger>, Boolean> {
        val parameterModes = getParameterModes(programState.second[programState.first].toInt())

        if (programState.third) return programState

        return when ((programState.second[programState.first] % 100.toBigInteger()).toInt()) {
            1 -> performOperation(programState, { a, b -> a + b }, parameterModes)
            2 -> performOperation(programState, { a, b -> a * b }, parameterModes)
            3 -> takeInput(programState, parameterModes)
            4 -> printOutput(programState, parameterModes)
            5 -> jumpIfTrue(programState, parameterModes)
            6 -> jumpIfFalse(programState, parameterModes)
            7 -> lessThan(programState, parameterModes)
            8 -> equals(programState, parameterModes)
            9 -> alterRelativeBase(programState, parameterModes)

            else -> Triple(programState.first, programState.second, true)
        }
    }

    private fun performOperation(programState: Triple<Int, List<BigInteger>, Boolean>, operation: (BigInteger, BigInteger) -> BigInteger, parameterModes: List<Int>): Triple<Int, List<BigInteger>, Boolean> {
        val (position, program) = programState

        val operand1 = getValue(program, position + 1, parameterModes[0])
        val operand2 = getValue(program, position + 2, parameterModes[1])
        val outputposition = getAddress(program, position + 3, parameterModes[2])

        return Triple(position + 4, program.replace(outputposition, operation(operand1, operand2)), false)
    }

    private fun takeInput(programState: Triple<Int, List<BigInteger>, Boolean>, parameterModes: List<Int>): Triple<Int, List<BigInteger>, Boolean> {
        val (position, program) = programState

        val input = inputProvider.getInput()

        return if (input == null) programState
        else Triple(position + 2, program.replace(getAddress(program, position + 1, parameterModes[0]), input), false)
    }

    private fun printOutput(programState: Triple<Int, List<BigInteger>, Boolean>, parameterModes: List<Int>): Triple<Int, List<BigInteger>, Boolean> {
        val (position, program) = programState
        val output = getValue(program, position + 1, parameterModes[0])

        outputConsumer.consumeOutput(output)

        return Triple(position + 2, program, false)
    }

    private fun jumpIfTrue(programState: Triple<Int, List<BigInteger>, Boolean>, parameterModes: List<Int>): Triple<Int, List<BigInteger>, Boolean> {
        val (position, program) = programState

        return if (getValue(program, position + 1, parameterModes[0]).toInt() != 0)
            Triple(getValue(program, position + 2, parameterModes[1]).toInt(), program, false)
        else Triple(position + 3, program, false)
    }

    private fun jumpIfFalse(programState: Triple<Int, List<BigInteger>, Boolean>, parameterModes: List<Int>): Triple<Int, List<BigInteger>, Boolean> {
        val (position, program) = programState

        return if (getValue(program, position + 1, parameterModes[0]).toInt() == 0)
            Triple(getValue(program, position + 2, parameterModes[1]).toInt(), program, false)
        else Triple(position + 3, program, false)
    }

    private fun lessThan(programState: Triple<Int, List<BigInteger>, Boolean>, parameterModes: List<Int>): Triple<Int, List<BigInteger>, Boolean> {
        val (position, program) = programState

        return if (getValue(program, position + 1, parameterModes[0]) < getValue(program, position + 2, parameterModes[1]))
            Triple(position + 4, program.replace(getAddress(program, position + 3, parameterModes[2]), 1), false)
        else Triple(position + 4, program.replace(getAddress(program, position + 3, parameterModes[2]), 0), false)
    }

    private fun equals(programState: Triple<Int, List<BigInteger>, Boolean>, parameterModes: List<Int>): Triple<Int, List<BigInteger>, Boolean> {
        val (position, program) = programState

        return if (getValue(program, position + 1, parameterModes[0]) == getValue(program, position + 2, parameterModes[1]))
            Triple(position + 4, program.replace(getAddress(program, position + 3, parameterModes[2]), 1), false)
        else Triple(position + 4, program.replace(getAddress(program, position + 3, parameterModes[2]), 0), false)
    }

    private fun alterRelativeBase(programState: Triple<Int, List<BigInteger>, Boolean>, parameterModes: List<Int>): Triple<Int, List<BigInteger>, Boolean> {
        val (position, program) = programState

        relativeBase += getValue(program, position + 1, parameterModes[0])

        return Triple(position + 2, program, false)
    }

    private fun getParameterModes(instruction: Int): List<Int> =
            (instruction / 100)
                    .toString()
                    .padStart(3, '0')
                    .toMutableList()
                    .map { it.toString().toInt() }
                    .reversed()

    private fun getValue(program: List<BigInteger>, position: Int, parameterMode: Int): BigInteger {
        val address = getAddress(program, position, parameterMode)

        return if (address >= program.count()) {
            0.toBigInteger()
        } else {
            program[address]
        }
    }

    private fun getAddress(program: List<BigInteger>, position: Int, parameterMode: Int): Int =
            when (parameterMode) {
                0 -> program[position].toInt()
                1 -> position
                2 -> (program[position] + relativeBase).toInt()
                else -> 0
            }

    private fun List<BigInteger>.replace(position: Int, newValue: BigInteger): List<BigInteger> {
        return if (position > this.count()) {
            this + List(position - this.count()) { 0.toBigInteger() } + listOf(newValue)
        } else {
            this.take(position) + listOf(newValue) + this.drop(position + 1)
        }
    }

    private fun List<BigInteger>.replace(position: Int, newValue: Int) =
            this.replace(position, newValue.toBigInteger())
}

