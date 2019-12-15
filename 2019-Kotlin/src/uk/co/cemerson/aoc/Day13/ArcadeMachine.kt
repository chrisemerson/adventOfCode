package uk.co.cemerson.aoc.Day13

import uk.co.cemerson.aoc.Util.IntCode.InputProvider
import uk.co.cemerson.aoc.Util.IntCode.OutputConsumer
import java.math.BigInteger

class ArcadeMachine : InputProvider, OutputConsumer {
    private var outputCount = 0
    private var grid = mutableListOf<Triple<BigInteger, BigInteger, Int>>()

    private var x = 0.toBigInteger()
    private var y = 0.toBigInteger()

    private var ballx = 0.toBigInteger()
    private var paddlex = 0.toBigInteger()

    private var score = 0

    override fun getInput(): BigInteger? {
        var move = 0

        if (paddlex < ballx) {
            move = 1
        } else if (paddlex > ballx) {
            move = -1
        }

        return move.toBigInteger()
    }

    override fun shouldHalt(): Boolean = false

    override fun consumeOutput(output: BigInteger) {
        when (outputCount) {
            0 -> collectXCoord(output)
            1 -> collectYCoord(output)
            2 -> collectTileType(output)
        }

        outputCount = (outputCount + 1) % 3
    }

    private fun collectXCoord(output: BigInteger) {
        x = output
    }

    private fun collectYCoord(output: BigInteger) {
        y = output
    }

    private fun collectTileType(output: BigInteger) {
        if (x == -1.toBigInteger() && y == 0.toBigInteger()) {
            score = output.toInt()
        } else {
            grid.add(Triple(x, y, output.toInt()))

            if (output.toInt() == 3) {
                paddlex = x
            }

            if (output.toInt() == 4) {
                ballx = x
            }
        }
    }

    fun getBlockTilesLeft(): Int = grid.filter { it.third == 2 }.count()

    fun getScore(): Int = score

    override fun consumeFinalValueInPositionZero(output: BigInteger) = Unit
}