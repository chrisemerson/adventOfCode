package uk.co.cemerson.aoc.Day2

import uk.co.cemerson.aoc.Util.IntCode.OutputConsumer
import java.math.BigInteger

class GetFinalProgramStateOutputConsumer : OutputConsumer {
    private var output: BigInteger = 0.toBigInteger()

    override fun consumeOutput(output: BigInteger) {
    }

    override fun consumeFinalValueInPositionZero(output: BigInteger) {
        this.output = output
    }

    fun getFinalOutput(): BigInteger = output
}