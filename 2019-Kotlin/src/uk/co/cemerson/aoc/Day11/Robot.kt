package uk.co.cemerson.aoc.Day11

import uk.co.cemerson.aoc.Util.IntCode.InputProvider
import uk.co.cemerson.aoc.Util.IntCode.OutputConsumer
import java.math.BigInteger

class Robot(val startingPanelColour: BigInteger) : InputProvider, OutputConsumer {
    var x = 0
    var y = 0
    var panelsPainted = 1 // 1 because we paint the first panel in the init
    var panelGrid = mutableListOf<Triple<Int, Int, BigInteger>>()
    var dir = 0 // N = 0, E = 1, S = 2, W = 3
    var oneOutputReceived = false
    var debug = false

    init {
        panelGrid.add(Triple(0, 0, startingPanelColour))
    }

    override fun getInput(): BigInteger? {
        return if (panelGrid.filter { it.first == x && it.second == y }.count() != 0) {
            val colour = panelGrid.filter { it.first == x && it.second == y }.map { it.third }.first()

            colour
        } else {
            0.toBigInteger()
        }
    }

    override fun consumeOutput(output: BigInteger) {
        if (!oneOutputReceived) {
            if (panelGrid.filter { it.first == x && it.second == y }.count() == 0) {
                panelsPainted++
                panelGrid.add(Triple(x, y, output))
            } else {
                panelGrid = (panelGrid.takeWhile { it.first != x || it.second != y } + listOf(Triple(x, y, output)) + panelGrid.takeLastWhile { it.first != x || it.second != y }).toMutableList()
            }
        } else {
            if (output.toInt() == 0) {
                dir = (dir + 3) % 4
            } else {
                dir = (dir + 1) % 4
            }

            when (dir) {
                0 -> y += 1
                1 -> x += 1
                2 -> y -= 1
                3 -> x -= 1
            }
        }

        oneOutputReceived = !oneOutputReceived
    }

    override fun consumeFinalValueInPositionZero(output: BigInteger) {
    }

    fun getFinalPanelPaintedCount(): Int {
        return panelsPainted
    }

    fun outputGrid() {
        val minx = panelGrid.map { it.first }.min()!!
        val miny = panelGrid.map { it.second }.min()!!
        val maxx = panelGrid.map { it.first }.max()!!
        val maxy = panelGrid.map { it.second }.max()!!

        for (y in maxy.downTo(miny)) {
            for (x in minx..maxx) {
                if (panelGrid.filter { it.first == x && it.second == y }.count() == 0) {
                    print(' ')
                } else {
                    if (panelGrid.filter { it.first == x && it.second == y }.first().third.toInt() == 0) {
                        print(' ')
                    } else {
                        print('#')
                    }
                }
            }

            println()
        }
    }
}

