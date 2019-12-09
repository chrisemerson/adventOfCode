package uk.co.cemerson.aoc.Util.IntCode

import java.math.BigInteger

class OutputCollector : OutputConsumer {
    private val output = mutableListOf<BigInteger>()

    override fun consumeOutput(output: BigInteger) {
        this.output.add(output)
    }

    override fun consumeFinalValueInPositionZero(output: BigInteger) {
    }

    fun getOutput(): List<BigInteger> = output
}