package uk.co.cemerson.aoc.Util.IntCode

import java.math.BigInteger

class ZeroInputProvider : InputProvider {
    override fun getInput(): BigInteger = 0.toBigInteger()
}