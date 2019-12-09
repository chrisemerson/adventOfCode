package uk.co.cemerson.aoc.Util.IntCode

import java.math.BigInteger

class PrintLnOutputConsumer : OutputConsumer {
    override fun consumeOutput(output: BigInteger) {
        println(output)
    }

    override fun consumeFinalValueInPositionZero(output: BigInteger) {
    }
}