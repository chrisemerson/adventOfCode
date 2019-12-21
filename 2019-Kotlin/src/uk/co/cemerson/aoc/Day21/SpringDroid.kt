package uk.co.cemerson.aoc.Day21

import uk.co.cemerson.aoc.Util.IntCode.InputProvider
import uk.co.cemerson.aoc.Util.IntCode.OutputConsumer
import java.math.BigInteger

class SpringDroid : InputProvider, OutputConsumer {
    private var input = mutableListOf<BigInteger>()

    fun provideWalkInstructions(instructions: List<String>) = provideInstructions(instructions + listOf("WALK"))

    fun provideRunInstructions(instructions: List<String>) = provideInstructions(instructions + listOf("RUN"))

    private fun provideInstructions(instructions: List<String>) {
        instructions.forEach {
            it.forEach {
                input.add(it.toInt().toBigInteger())
            }

            input.add(10.toBigInteger())
        }
    }

    override fun getInput(): BigInteger? {
        val inputValue = input.first()
        input = input.drop(1).toMutableList()

        return inputValue
    }

    override fun shouldHalt(): Boolean = false

    override fun consumeOutput(output: BigInteger) {
        if (output > 255.toBigInteger()) {
            println("Damage to hull: " + output)
        } else {
            print(output.toInt().toChar())
        }
    }

    override fun consumeFinalValueInPositionZero(output: BigInteger) = Unit
}