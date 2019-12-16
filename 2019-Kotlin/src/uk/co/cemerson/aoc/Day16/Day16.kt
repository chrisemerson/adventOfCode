package uk.co.cemerson.aoc.Day16

import uk.co.cemerson.aoc.AOCDay
import java.lang.Math.abs

class Day16 : AOCDay {
    override fun part1() =
            println(run100Phases(getInput(), this::iterateOnePhase))

    override fun part2() {
        val input = getInput().repeat(10000)

        println(run100Phases(
                input.drop(input.take(7).toInt()),
                this::iterateOnePhasePt2
        ))
    }

    private fun run100Phases(input: String, callBack: (String) -> String): String =
            generateSequence(callBack(input)) { callBack(it) }
                    .map { it.take(8) }
                    .take(100)
                    .last()

    private fun iterateOnePhase(number: String): String =
            number
                    .mapIndexed { index, _ -> calculateSingleDigit(number, index + 1) }
                    .joinToString("")

    private fun iterateOnePhasePt2(number: String): String {
        //Take advantage of known properties about position and number length to shortcut process!
        val result = mutableListOf<Int>()
        val reversedNumber = number.toCharArray().reversed()

        for (i in reversedNumber.indices) {
            if (i == 0) {
                result.add(reversedNumber[i].toString().toInt() % 10)
            } else {
                result.add((result[i - 1] + reversedNumber[i].toString().toInt()) % 10)
            }
        }

        return result.reversed().joinToString("")
    }

    private fun calculateSingleDigit(number: String, position: Int): Int {
        //Drop the first <position - 1> digits, they all get multiplied by 0 so have no effect
        val numberAsInts = number.drop(position - 1).map { it.toString().toInt() }

        val answer = if (position > number.length / 3) {
            //Shortcut when position is more than 1/3 the way along input
            // - just sum the next <position> digits
            numberAsInts.take(position).sum()
        } else {
            numberAsInts

                    //Chunk up by position, as this is the length of each element in the pattern
                    .chunk(position)

                    //Then chunk into groups of 4 position-long lists for repeated pattern
                    .chunk(4)

                    //Because we dropped the first (position -1) digits, pattern is shifted
                    // - is effectively now 1, 0, -1, 0

                    //This is equivalent to summing the first group, then taking off the sum
                    //of the third group, if it exists
                    .map { it.first().sum() - (it.drop(2).firstOrNull()?.sum() ?: 0) }

                    //Sum the results of each pattern repetition
                    .sum()
        }

        //Take the absolute value of the answer and return the last digit as a string
        return abs(answer) % 10
    }

    private fun getInput(): String = readFileContents("Day16/input.txt")

    private fun <T> List<T>.chunk(size: Int): List<List<T>> =
            this.withIndex().groupBy { it.index / size }.map { it.value.map { it.value } }
}