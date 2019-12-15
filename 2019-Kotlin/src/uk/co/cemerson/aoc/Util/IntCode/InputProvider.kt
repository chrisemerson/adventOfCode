package uk.co.cemerson.aoc.Util.IntCode

import java.math.BigInteger

interface InputProvider {
    fun getInput(): BigInteger?
    fun shouldHalt(): Boolean
}