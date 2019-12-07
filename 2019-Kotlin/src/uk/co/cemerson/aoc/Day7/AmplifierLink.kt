package uk.co.cemerson.aoc.Day7

import uk.co.cemerson.aoc.Util.IntCode.InputProvider
import uk.co.cemerson.aoc.Util.IntCode.OutputConsumer

class AmplifierLink(initialInput: List<Int>? = null) : InputProvider, OutputConsumer {
    private var signals = mutableListOf<Int>()

    init {
        if (initialInput != null) {
            this.signals = initialInput.toMutableList()
        }
    }

    override fun getInput(): Int? {
        if (signals.isEmpty()) return null

        val input = signals.first()
        signals = signals.drop(1).toMutableList()

        return input
    }

    override fun consumeOutput(output: Int) {
        signals.add(output)
    }

    override fun consumeFinalValueInPositionZero(output: Int) {
    }

    fun getOutput(): List<Int> = signals
}