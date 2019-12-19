package uk.co.cemerson.aoc.Day19

import uk.co.cemerson.aoc.Util.IntCode.InputProvider
import uk.co.cemerson.aoc.Util.IntCode.OutputConsumer
import java.math.BigInteger

class DroneSystem(var startX: Int, var startY: Int, val step: Int) : InputProvider, OutputConsumer {
    private var x = startX
    private var y = startY

    private var lastOutputCoord = Pair(0, 0)

    private var returnY = false

    private val tractorBeamGrid = mutableMapOf<Pair<Int, Int>, Boolean>()

    override fun getInput(): BigInteger? {
        val input = if (returnY) y else x

        if (returnY) {
            lastOutputCoord = Pair(x, y)

            if ((x - startX) > step) {
                x = startX
                y += 1
            } else {
                x += 1
            }
        }

        returnY = !returnY

        return input.toBigInteger()
    }

    override fun shouldHalt(): Boolean = false

    override fun consumeOutput(output: BigInteger) {
        tractorBeamGrid[lastOutputCoord] = (output.toInt() == 1)
    }

    override fun consumeFinalValueInPositionZero(output: BigInteger) = Unit

    fun getNumberOfAffectedPoints(): Int = tractorBeamGrid.filter { it.value }.count()

    fun drawGrid() {
        val minX = tractorBeamGrid.map { it.key.first }.min()!!
        val minY = tractorBeamGrid.map { it.key.second }.min()!!
        val maxX = tractorBeamGrid.map { it.key.first }.max()!!
        val maxY = tractorBeamGrid.map { it.key.second }.max()!!

        println("X: " + minX + " to " + maxX)
        println("Y: " + minY + " to " + maxY)


        for (y in minY..maxY) {
            for (x in minX..maxX) {
                if (tractorBeamGrid[Pair(x, y)] == true) {
                    print('#')
                } else {
                    print('.')
                }
            }

            println()
        }
    }
}