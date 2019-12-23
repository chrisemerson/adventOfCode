package uk.co.cemerson.aoc.Day23

import uk.co.cemerson.aoc.Util.IntCode.InputProvider
import java.math.BigInteger

class InputInterface : InputProvider {
    private var inputQueue = mutableListOf<BigInteger>()

    fun addToQueue(input: BigInteger) {
        inputQueue.add(input)
    }

    override fun getInput(): BigInteger? {
        if (inputQueue.count() == 0) {
            return -1.toBigInteger()
        }

        val input = inputQueue.first()
        inputQueue = inputQueue.drop(1).toMutableList()

        return input
    }

    override fun shouldHalt(): Boolean = false

    fun isIdle(): Boolean = inputQueue.count() == 0
}