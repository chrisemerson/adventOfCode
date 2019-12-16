package uk.co.cemerson.aoc.Day16

import uk.co.cemerson.aoc.AOCDay
import java.lang.Math.abs

class Day16 : AOCDay {
    override fun part1() {
        val input = getInput()

        val answer = generateSequence(iterateOnePhase(input)) { iterateOnePhase(it) }
                .map { it.take(8) }
                .take(100)
                .last()

        println(answer)
    }

    override fun part2() {
        var input = getInput().repeat(10000)
        val positionToSkipTo = input.take(7).toInt()

        input = input.drop(positionToSkipTo)

        for (i in 1..100) {
            input = iterateOnePhasePt2(input)
        }

        val answer = input
                .take(8)

        println(answer)
    }

    private fun iterateOnePhase(number: String): String {
        val result = mutableListOf<String>()

        number.forEachIndexed { index, _ -> result.add(calculateSingleDigit(number, index + 1)) }

        return result.joinToString("")
    }

    private fun iterateOnePhasePt2(number: String): String {
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

    private fun calculateSingleDigit(number: String, position: Int): String {
        val answer: Int

        if (position > number.length / 2) {
            answer = number
                    .drop(position - 1)
                    .map { it.toString().toInt() }
                    .reduce { a, b -> a + b }
        } else if (position > number.length / 3) {
            answer = number
                    .drop(position - 1)
                    .take(position)
                    .map { it.toString().toInt() }
                    .reduce { a, b -> a + b }
        } else {
            //Drop first position - 1 chars (will be multiplied by 0 anyway), group rest into groups of position long
            val numberGroups = number
                    .drop(position - 1)
                    .withIndex()
                    .groupBy { it.index / position }
                    .map { it.value.map { it.value } }

            //Group again into groups of 4 for repeating pattern
            val result = numberGroups
                    .withIndex()
                    .groupBy { it.index / 4 }
                    .map { it.value.map { it.value } }

            answer = result
                    .map {
                        if (it.count() < 3) {
                            addCharList(it.first())
                        } else {
                            (addCharList(it.first()) - addCharList(it.take(3).last()))
                        }
                    }
                    .reduce { a, b -> a + b }
        }

        return (abs(answer) % 10).toString()
    }

    private fun addCharList(list: List<Char>): Int {
        return list.map { it.toString().toInt() }.reduce { a, b -> a + b }
    }

    private fun getInput(): String {
        return readFileContents("Day16/input.txt")
    }
}