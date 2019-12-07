package uk.co.cemerson.aoc.Util.IntCode

class PrintLnOutputConsumer : OutputConsumer {
    override fun consumeOutput(output: Int) {
        println(output)
    }

    override fun consumeFinalValueInPositionZero(output: Int) {
    }
}