package uk.co.cemerson.aoc.Day7

import uk.co.cemerson.aoc.Util.IntCode.InputProvider
import uk.co.cemerson.aoc.Util.IntCode.OutputConsumer
import java.math.BigInteger

class AmplifierLink(initialInput: List<BigInteger>? = null) : InputProvider, OutputConsumer {
    private var signals = mutableListOf<BigInteger>()

    init {
        if (initialInput != null) {
            this.signals = initialInput.toMutableList()
        }
    }

    override fun getInput(): BigInteger? {
        if (signals.isEmpty()) return null

        val input = signals.first()
        signals = signals.drop(1).toMutableList()

        return input
    }

    override fun shouldHalt(): Boolean = false

    override fun consumeOutput(output: BigInteger) {
        signals.add(output)
    }

    override fun consumeFinalValueInPositionZero(output: BigInteger) {
    }

    fun getOutput(): List<BigInteger> = signals
}