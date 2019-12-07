package uk.co.cemerson.aoc.Util.IntCode

interface OutputConsumer {
    fun consumeOutput(output: Int)
    fun consumeFinalValueInPositionZero(output: Int)
}