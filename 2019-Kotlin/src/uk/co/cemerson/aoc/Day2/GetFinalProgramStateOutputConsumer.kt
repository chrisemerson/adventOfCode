package uk.co.cemerson.aoc.Day2

import uk.co.cemerson.aoc.Util.IntCode.OutputConsumer

class GetFinalProgramStateOutputConsumer : OutputConsumer {
    private var output: Int = 0

    override fun consumeOutput(output: Int) {
    }

    override fun consumeFinalValueInPositionZero(output: Int) {
        this.output = output
    }

    fun getFinalOutput(): Int = output
}