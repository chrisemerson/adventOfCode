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
        //Because position is well over half way into the input, we know we are past the 0s in the pattern, and that all that are left must be 1s
        // - ie, we can not bother with a pattern at all, since multiplying each digit by 1 is itself
        val result = mutableListOf<Int>()

        //Because each digit in the output depends only on the digit in the same position in the input and all subsequent digits, better to work backwards for linear time algorithm
        val reversedNumber = number.toCharArray().reversed()

        for (i in reversedNumber.indices) {
            if (i == 0) {
                //Special case for last digit - just copy across
                result.add(reversedNumber[i].toString().toInt())
            } else {
                //For any other digit, add the previous output digit to the current input digit and mod 10
                result.add((result[i - 1] + reversedNumber[i].toString().toInt()) % 10)
            }
        }

        //Reverse the result again to get the output the right way around
        return result.reversed().joinToString("")
    }

    private fun calculateSingleDigit(number: String, position: Int): Int {
        //Drop the first <position - 1> digits, they all get multiplied by 0 so have no effect
        val numberAsInts = number.drop(position - 1).map { it.toString().toInt() }

        val answer = if (position > number.length / 3) {
            //Shortcut when position is more than 1/3 the way along input - just sum the next <position> digits
            numberAsInts.take(position).sum()
        } else {
            numberAsInts
                    //Chunk up by position, as this is the length of each element in the pattern
                    //For position p, this gives... [[a, b, c, d, e, f,... p], [g, h, i, j, k, l,... p], ...
                    .chunk(position)

                    //Then chunk into groups of 4 position-long lists for repeated pattern
                    //For position p, this gives... [[[a, b, c,... p], [d, e, f,... p], [g, h, i,... p], [j, k, l,... p]], [[a2, b2, c2,... p], ...
                    //                               |--------------------------- Group of 4 ---------------------------|  |-----------------------
                    .chunk(4)

                    //Because we dropped the first (position -1) digits, pattern is shifted - is effectively now 1, 0, -1, 0
                    //This is equivalent to summing the first group in each group of 4, then taking off the sum of the third, if it exists
                    .map { it.first().sum() - (it.drop(2).firstOrNull()?.sum() ?: 0) }

                    //Sum the results of each pattern repetition
                    .sum()
        }

        //Return the last digit of the answer
        return abs(answer) % 10
    }

    private fun getInput(): String = readFileContents("Day16/input.txt")

    private fun <T> List<T>.chunk(size: Int): List<List<T>> =
            this.withIndex().groupBy { it.index / size }.map { it.value.map { it.value } }
}