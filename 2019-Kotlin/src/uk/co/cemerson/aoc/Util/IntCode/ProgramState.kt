package uk.co.cemerson.aoc.Util.IntCode

import java.math.BigInteger

data class ProgramState(
        val position: Int,
        val program: List<BigInteger>,
        val relativeBase: BigInteger,
        val halted: Boolean
)