package uk.co.cemerson.aoc.Day21

import uk.co.cemerson.aoc.AOCDay
import uk.co.cemerson.aoc.Util.IntCode.Computer
import java.math.BigInteger

class Day21 : AOCDay {
    override fun part1() {
        val program = getProgram()
        val springDroid = SpringDroid()
        val computer = Computer(springDroid, springDroid)

        springDroid.provideWalkInstructions(getSpringScriptPt1())

        computer.execute(program)

        println("Done")
    }

    override fun part2() {
        val program = getProgram()
        val springDroid = SpringDroid()
        val computer = Computer(springDroid, springDroid)

        springDroid.provideRunInstructions(getSpringScriptPt2())

        computer.execute(program)

        println("Done")
    }

    private fun getProgram(): List<BigInteger> =
            readFileSplitByChar(filename = "Day21/input.txt", splitBy = ',')
                    .map { it.toBigInteger() }

    private fun getSpringScriptPt1(): List<String> = getSpringScript("Day21/springscript.txt")

    private fun getSpringScriptPt2(): List<String> = getSpringScript("Day21/springscriptpt2.txt")

    private fun getSpringScript(filename: String): List<String> =
            readFileLines(filename)
                    .filter { it.trim().first() != '#' }
}