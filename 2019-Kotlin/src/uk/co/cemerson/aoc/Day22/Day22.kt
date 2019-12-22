package uk.co.cemerson.aoc.Day22

import uk.co.cemerson.aoc.AOCDay
import java.lang.Math.abs

class Day22 : AOCDay {
    override fun part1() {
        val instructions = getInput()
        val shuffledCards = processInstructions(instructions, (0..10006).toList())

        println(shuffledCards.withIndex().filter { it.value == 2019 }.first().index)
    }

    override fun part2() {


    }

    private fun processInstructions(instructions: List<String>, initialDeck: List<Int>): List<Int> =
            instructions.fold(initialDeck) { deck, instruction -> processInstruction(instruction, deck) }

    private fun processInstruction(instruction: String, deck: List<Int>): List<Int> {
        if (instruction == "deal into new stack") {
            return dealIntoNewStack(deck)
        }

        val cutMatchResult = "cut (-?\\d+)".toRegex().matchEntire(instruction)
        if (cutMatchResult != null) {
            return cutCards(deck, cutMatchResult.groups[1]!!.value.toInt())
        }

        val dealIncrementMatchResult = "deal with increment (\\d+)".toRegex().matchEntire(instruction)
        if (dealIncrementMatchResult != null) {
            return dealWithIncrement(deck, dealIncrementMatchResult.groups[1]!!.value.toInt())
        }

        return deck
    }

    private fun dealIntoNewStack(deck: List<Int>): List<Int> {
        println("Dealing into new stack")
        return deck.reversed()
    }

    private fun cutCards(deck: List<Int>, cutAmount: Int): List<Int> {
        println("Cutting " + cutAmount + " cards")

        return if (cutAmount < 0) deck.takeLast(abs(cutAmount)) + deck.take(deck.count() + cutAmount)
        else deck.drop(cutAmount) + deck.take(cutAmount)
    }

    private fun dealWithIncrement(deck: List<Int>, increment: Int): List<Int> {
        println("Deal with increment " + increment)
        val outputDeck = mutableMapOf<Int, Int>()

        for (i in 0.until(deck.count())) {
            outputDeck[(i * increment) % deck.count()] = deck[i]
        }

        return outputDeck.toSortedMap().toList().map { it.second }
    }

    private fun getInput(): List<String> =
            readFileLines("Day22/input.txt")
}