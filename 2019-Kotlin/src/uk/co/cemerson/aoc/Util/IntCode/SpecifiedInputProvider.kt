package uk.co.cemerson.aoc.Util.IntCode

import java.math.BigInteger

class SpecifiedInputProvider(private var input: List<Int>) : InputProvider {
    override fun getInput(): BigInteger {
        val input = this.input.first()
        this.input = this.input.drop(1)

        return input.toBigInteger()
    }

    override fun shouldHalt(): Boolean = false
}