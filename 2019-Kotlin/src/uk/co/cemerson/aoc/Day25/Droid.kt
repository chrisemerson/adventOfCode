package uk.co.cemerson.aoc.Day25

import uk.co.cemerson.aoc.Util.IntCode.InputProvider
import uk.co.cemerson.aoc.Util.IntCode.OutputConsumer
import java.math.BigInteger

class Droid : InputProvider, OutputConsumer {
    private var input = listOf<BigInteger>()

    override fun getInput(): BigInteger? {
        val thisInput: BigInteger

        if (input.count() == 0) {
            val command = readLine()!!

            if (command.isNotEmpty()) {
                thisInput = command.first().toInt().toBigInteger()

                input = command.drop(1).map { it.toInt().toBigInteger() } + listOf(10.toBigInteger())
            } else {
                return null
            }
        } else {
            thisInput = input.first()
            input = input.drop(1)
        }

        return thisInput
    }

    override fun shouldHalt(): Boolean = false

    override fun consumeOutput(output: BigInteger) {
        print(output.toInt().toChar())
    }

    override fun consumeFinalValueInPositionZero(output: BigInteger) = Unit
}