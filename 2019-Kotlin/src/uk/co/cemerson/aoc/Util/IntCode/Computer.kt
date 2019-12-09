package uk.co.cemerson.aoc.Util.IntCode

import java.math.BigInteger

class Computer(private val inputProvider: InputProvider, private val outputConsumer: OutputConsumer) {
    fun execute(program: List<BigInteger>): Unit =
            outputConsumer.consumeFinalValueInPositionZero(
                    generateSequence(getInitialProgramState(program)) { executeProgramStep(it) }
                            .takeWhile { !it.halted }
                            .map { it.program[0] }
                            .last()
            )

    fun executeProgramStep(programState: ProgramState): ProgramState {
        val parameterModes = getParameterModes(programState.program[programState.position])

        if (programState.halted) return programState

        return when ((programState.program[programState.position] % 100.toBigInteger()).toInt()) {
            1 -> performOperation(programState, { a, b -> a + b }, parameterModes)
            2 -> performOperation(programState, { a, b -> a * b }, parameterModes)
            3 -> takeInput(programState, parameterModes)
            4 -> printOutput(programState, parameterModes)
            5 -> jumpIfTrue(programState, parameterModes)
            6 -> jumpIfFalse(programState, parameterModes)
            7 -> lessThan(programState, parameterModes)
            8 -> equals(programState, parameterModes)
            9 -> alterRelativeBase(programState, parameterModes)

            else -> ProgramState(programState.position, programState.program, programState.relativeBase, true)
        }
    }

    fun getInitialProgramState(program: List<BigInteger>): ProgramState =
            ProgramState(0, program, 0.toBigInteger(), false)

    private fun performOperation(programState: ProgramState, operation: (BigInteger, BigInteger) -> BigInteger, parameterModes: List<Int>): ProgramState =
            ProgramState(
                    programState.position + 4,
                    programState.program.replace(
                            getAddress(programState.program, programState.position + 3, programState.relativeBase, parameterModes[2]),
                            operation(
                                    getValue(programState.program, programState.position + 1, programState.relativeBase, parameterModes[0]),
                                    getValue(programState.program, programState.position + 2, programState.relativeBase, parameterModes[1])
                            )
                    ),
                    programState.relativeBase,
                    false
            )

    private fun takeInput(programState: ProgramState, parameterModes: List<Int>): ProgramState {
        val input = inputProvider.getInput()

        return if (input == null) programState
        else ProgramState(
                programState.position + 2,
                programState.program.replace(
                        getAddress(programState.program, programState.position + 1, programState.relativeBase, parameterModes[0]),
                        input
                ),
                programState.relativeBase,
                false
        )
    }

    private fun printOutput(programState: ProgramState, parameterModes: List<Int>): ProgramState {
        outputConsumer.consumeOutput(
                getValue(programState.program, programState.position + 1, programState.relativeBase, parameterModes[0])
        )

        return ProgramState(
                programState.position + 2,
                programState.program,
                programState.relativeBase,
                false
        )
    }

    private fun jumpIfTrue(programState: ProgramState, parameterModes: List<Int>): ProgramState =
            if (getValue(programState.program, programState.position + 1, programState.relativeBase, parameterModes[0]).toInt() != 0)
                ProgramState(
                        getValue(programState.program, programState.position + 2, programState.relativeBase, parameterModes[1]).toInt(),
                        programState.program,
                        programState.relativeBase,
                        false
                )
            else ProgramState(
                    programState.position + 3,
                    programState.program,
                    programState.relativeBase,
                    false
            )

    private fun jumpIfFalse(programState: ProgramState, parameterModes: List<Int>): ProgramState =
            if (getValue(programState.program, programState.position + 1, programState.relativeBase, parameterModes[0]).toInt() == 0)
                ProgramState(
                        getValue(programState.program, programState.position + 2, programState.relativeBase, parameterModes[1]).toInt(),
                        programState.program,
                        programState.relativeBase,
                        false
                )
            else ProgramState(
                    programState.position + 3,
                    programState.program,
                    programState.relativeBase,
                    false
            )

    private fun lessThan(programState: ProgramState, parameterModes: List<Int>): ProgramState =
            if (getValue(programState.program, programState.position + 1, programState.relativeBase, parameterModes[0]) < getValue(programState.program, programState.position + 2, programState.relativeBase, parameterModes[1]))
                ProgramState(
                        programState.position + 4,
                        programState.program.replace(getAddress(programState.program, programState.position + 3, programState.relativeBase, parameterModes[2]), 1),
                        programState.relativeBase,
                        false
                )
            else ProgramState(
                    programState.position + 4,
                    programState.program.replace(getAddress(programState.program, programState.position + 3, programState.relativeBase, parameterModes[2]), 0),
                    programState.relativeBase,
                    false
            )

    private fun equals(programState: ProgramState, parameterModes: List<Int>): ProgramState =
            if (getValue(programState.program, programState.position + 1, programState.relativeBase, parameterModes[0]) == getValue(programState.program, programState.position + 2, programState.relativeBase, parameterModes[1]))
                ProgramState(
                        programState.position + 4,
                        programState.program.replace(getAddress(programState.program, programState.position + 3, programState.relativeBase, parameterModes[2]), 1),
                        programState.relativeBase,
                        false
                )
            else ProgramState(
                    programState.position + 4,
                    programState.program.replace(getAddress(programState.program, programState.position + 3, programState.relativeBase, parameterModes[2]), 0),
                    programState.relativeBase,
                    false
            )

    private fun alterRelativeBase(programState: ProgramState, parameterModes: List<Int>): ProgramState =
            ProgramState(
                    programState.position + 2,
                    programState.program,
                    programState.relativeBase + getValue(programState.program, programState.position + 1, programState.relativeBase, parameterModes[0]),
                    false)

    private fun getParameterModes(instruction: BigInteger): List<Int> =
            (instruction / 100.toBigInteger())
                    .toString()
                    .padStart(3, '0')
                    .toMutableList()
                    .map { it.toString().toInt() }
                    .reversed()

    private fun getValue(program: List<BigInteger>, position: Int, relativeBase: BigInteger, parameterMode: Int): BigInteger {
        val address = getAddress(program, position, relativeBase, parameterMode)

        return if (address >= program.count()) {
            0.toBigInteger()
        } else {
            program[address]
        }
    }

    private fun getAddress(program: List<BigInteger>, position: Int, relativeBase: BigInteger, parameterMode: Int): Int =
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

