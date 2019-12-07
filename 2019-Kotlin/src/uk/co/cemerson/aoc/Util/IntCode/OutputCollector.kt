package uk.co.cemerson.aoc.Util.IntCode

class OutputCollector : OutputConsumer {
    private val output = mutableListOf<Int>()

    override fun consumeOutput(output: Int) {
        this.output.add(output)
    }

    override fun consumeFinalValueInPositionZero(output: Int) {
    }

    fun getOutput(): List<Int> = output
}