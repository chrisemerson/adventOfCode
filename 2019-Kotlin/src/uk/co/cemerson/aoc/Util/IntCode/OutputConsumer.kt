package uk.co.cemerson.aoc.Util.IntCode

import java.math.BigInteger

interface OutputConsumer {
    fun consumeOutput(output: BigInteger)
    fun consumeFinalValueInPositionZero(output: BigInteger)
}