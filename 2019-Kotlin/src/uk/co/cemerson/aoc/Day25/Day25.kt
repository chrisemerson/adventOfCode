package uk.co.cemerson.aoc.Day25

import uk.co.cemerson.aoc.AOCDay
import uk.co.cemerson.aoc.Util.IntCode.Computer
import java.math.BigInteger

class Day25 : AOCDay {
    override fun part1() {
        val droid = Droid()
        val computer = Computer(droid, droid)
        val program = getProgram()

        computer.execute(program)
    }

    override fun part2() {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    private fun getProgram(): List<BigInteger> =
            readFileSplitByChar(filename = "Day25/input.txt", splitBy = ',')
                    .map { it.toBigInteger() }
}