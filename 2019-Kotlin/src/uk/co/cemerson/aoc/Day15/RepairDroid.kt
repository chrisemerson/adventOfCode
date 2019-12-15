package uk.co.cemerson.aoc.Day15

import uk.co.cemerson.aoc.Util.IntCode.InputProvider
import uk.co.cemerson.aoc.Util.IntCode.OutputConsumer
import java.math.BigInteger

class RepairDroid(val part2: Boolean) : InputProvider, OutputConsumer {
    private var currentDirection = 1 // 1 = N, 2 = S, 3 = W, 4 = E
    private var lastDirection = 1
    private var probingDirections = true

    private var x = 0
    private var y = 0

    private val maze = mutableMapOf<Pair<Int, Int>, Boolean>()
    private var oxygenSystemLocation = Pair(0, 0)

    private val distancesFromHome = mutableMapOf(Pair(0, 0) to 0)
    private var currentDistanceFromHome = 0

    override fun getInput(): BigInteger? {
        //Try to stick to left wall
        if (!probingDirections && currentDirection == lastDirection) {
            probingDirections = true
            currentDirection = changeDirection(currentDirection, 'L')
        } else {
            currentDirection = changeDirection(currentDirection, 'R')
        }

        return currentDirection.toBigInteger()
    }

    override fun shouldHalt(): Boolean = (!part2 && foundOxygenSystem()) || (part2 && fullyExploredMap())

    private fun foundOxygenSystem() = oxygenSystemLocation.first != 0 || oxygenSystemLocation.second != 0

    private fun fullyExploredMap(): Boolean = foundOxygenSystem() && maze.map{it.value}.filter{!it}.count() == distancesFromHome.count()

    override fun consumeOutput(output: BigInteger) {
        when (output.toInt()) {
            0 -> {
                //Wall
                when (currentDirection) {
                    1 -> maze.put(Pair(x, y + 1), true)
                    2 -> maze.put(Pair(x, y - 1), true)
                    3 -> maze.put(Pair(x - 1, y), true)
                    4 -> maze.put(Pair(x + 1, y), true)
                }
            }

            1, 2 -> {
                //Open space
                lastDirection = currentDirection
                probingDirections = false

                when (currentDirection) {
                    1 -> y++
                    2 -> y--
                    3 -> x--
                    4 -> x++
                }

                maze.put(Pair(x, y), false)

                if (!part2 || foundOxygenSystem()) {
                    if (distancesFromHome.filter { it.key.first == x && it.key.second == y }.count() == 0) {
                        currentDistanceFromHome++
                        distancesFromHome.put(Pair(x, y), currentDistanceFromHome)
                    } else {
                        //Visited this space before
                        currentDistanceFromHome = distancesFromHome.filter { it.key.first == x && it.key.second == y }.map { it.value }.first()
                    }
                }

                if (output.toInt() == 2) {
                    oxygenSystemLocation = Pair(x, y)
                }
            }
        }
    }

    override fun consumeFinalValueInPositionZero(output: BigInteger) = Unit

    fun getMaze(): MutableMap<Pair<Int, Int>, Boolean> = maze

    fun getDistancesFromHome(): MutableMap<Pair<Int, Int>, Int> = distancesFromHome

    fun getOxygenSystemLocation(): Pair<Int, Int> = oxygenSystemLocation

    private fun changeDirection(currentDirection: Int, directionToChange: Char): Int {
        return when (directionToChange) {
            'L' -> when (currentDirection) {
                1 -> 3
                2 -> 4
                3 -> 2
                4 -> 1
                else -> currentDirection
            }

            'R' -> when (currentDirection) {
                1 -> 4
                2 -> 3
                3 -> 1
                4 -> 2
                else -> currentDirection
            }

            else -> currentDirection
        }
    }
}