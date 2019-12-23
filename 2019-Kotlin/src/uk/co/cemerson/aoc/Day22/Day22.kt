package uk.co.cemerson.aoc.Day22

import uk.co.cemerson.aoc.AOCDay
import java.math.BigInteger


class Day22 : AOCDay {
    override fun part1() {
        val deckSize = 10007.toBigInteger()
        val totalCoeffs = getCoefficientsOfLinearTransformForSingleShuffle(deckSize)

        println("Index of card 2019: " + applyModularLinearTransformToSingleNumber(totalCoeffs.first, totalCoeffs.second, deckSize, 2019.toBigInteger()))
    }

    override fun part2() {
        val deckSize = "119315717514047".toBigInteger()
        val shuffles = "101741582076661".toBigInteger()
        val coeffsForSingleShuffle = getCoefficientsOfLinearTransformForSingleShuffle(deckSize)

        val coeffsForPowersOfTwo = mutableMapOf(0 to coeffsForSingleShuffle)

        for (i in 1..50) {
            val coeffsForPowerOfTwoBeforeThisOne = coeffsForPowersOfTwo[i - 1]!!
            val coeffsForThisPowerOfTwo = combineLinearFunctions(listOf(coeffsForPowerOfTwoBeforeThisOne, coeffsForPowerOfTwoBeforeThisOne))

            coeffsForPowersOfTwo[i] = Pair(coeffsForThisPowerOfTwo.first % deckSize, coeffsForThisPowerOfTwo.second % deckSize)
        }

        val coeffsForThisShuffle = mutableListOf<Pair<BigInteger, BigInteger>>()

        for (i in 0..50) {
            val bit = 2.toBigInteger().pow(i)

            if (shuffles and bit != 0.toBigInteger()) {
                coeffsForThisShuffle.add(coeffsForPowersOfTwo[i]!!)
            }
        }

        val totalCoeffs = combineLinearFunctions(coeffsForThisShuffle)
        val moduloInverse = totalCoeffs.first.modInverse(deckSize)
        val cardInPosition2020 = ((((2020.toBigInteger() + deckSize - totalCoeffs.second) * moduloInverse) % deckSize) + deckSize) % deckSize

        println("Card in position 2020: " + cardInPosition2020)
        println("Position of card " + cardInPosition2020 + ": " + applyModularLinearTransformToSingleNumber(totalCoeffs.first, totalCoeffs.second, deckSize, cardInPosition2020))
    }

    private fun getCoefficientsOfLinearTransformForSingleShuffle(deckSize: BigInteger): Pair<BigInteger, BigInteger> {
        val instructions = getInput()
        val linearCoeffs = convertInstructionsToCoefficientsForLinearEquation(instructions)
        val totalCoeffs = combineLinearFunctions(linearCoeffs)

        return Pair(totalCoeffs.first % deckSize, totalCoeffs.second % deckSize)
    }

    private fun convertInstructionsToCoefficientsForLinearEquation(instructions: List<String>): List<Pair<BigInteger, BigInteger>> =
            instructions.map { convertSingleInstructionToCoefficientsForLinearEquation(it) }

    private fun convertSingleInstructionToCoefficientsForLinearEquation(instruction: String): Pair<BigInteger, BigInteger> {
        if (instruction == "deal into new stack") {
            return Pair(-1.toBigInteger(), -1.toBigInteger())
        }

        val cutMatchResult = "cut (-?\\d+)".toRegex().matchEntire(instruction)
        if (cutMatchResult != null) {
            return Pair(1.toBigInteger(), 0.toBigInteger() - cutMatchResult.groups[1]!!.value.toBigInteger())
        }

        val dealIncrementMatchResult = "deal with increment (\\d+)".toRegex().matchEntire(instruction)
        if (dealIncrementMatchResult != null) {
            return Pair(dealIncrementMatchResult.groups[1]!!.value.toBigInteger(), 0.toBigInteger())
        }

        return Pair(1.toBigInteger(), 0.toBigInteger())
    }

    private fun combineLinearFunctions(functions: List<Pair<BigInteger, BigInteger>>): Pair<BigInteger, BigInteger> =
            functions
                    .reduce { a: Pair<BigInteger, BigInteger>, b: Pair<BigInteger, BigInteger> -> Pair(a.first * b.first, b.first * a.second + b.second) }

    private fun applyModularLinearTransformToSingleNumber(a: BigInteger, b: BigInteger, n: BigInteger, x: BigInteger): BigInteger =
            (((a * x + b) % n) + n) % n

    private fun getInput(): List<String> =
            readFileLines("Day22/input.txt")
}