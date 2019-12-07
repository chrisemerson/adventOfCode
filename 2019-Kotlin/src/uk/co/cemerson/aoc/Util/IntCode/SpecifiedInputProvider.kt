package uk.co.cemerson.aoc.Util.IntCode

class SpecifiedInputProvider(private var input: List<Int>) : InputProvider {
    override fun getInput(): Int {
        val input = this.input.first()
        this.input = this.input.drop(1)

        return input
    }
}