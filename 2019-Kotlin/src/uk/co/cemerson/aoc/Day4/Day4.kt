package uk.co.cemerson.aoc.Day4

import uk.co.cemerson.aoc.AOCDay

class Day4 : AOCDay {
    override fun part1() {
        val passwords = getInputRange()
                .filter { it.hasDigitsInAscendingOrEqualOrder() }
                .filter { it.hasAtLeastTwoAdjacentIdenticalDigits() }
                .count()

        println("Number of passwords: " + passwords)
    }

    override fun part2() {
        val passwords = getInputRange()
                .filter { it.hasDigitsInAscendingOrEqualOrder() }
                .filter { it.hasExactlyTwoAdjacentIdenticalDigits() }
                .count()

        println("Number of passwords: " + passwords)
    }

    private fun getInputRange() =
            (172930..683082)
                    .map { it.toString() }

    private fun String.hasDigitsInAscendingOrEqualOrder() =
            this[0] <= this[1] && this[1] <= this[2] && this[2] <= this[3] && this[3] <= this[4] && this[4] <= this[5]

    private fun String.hasExactlyTwoAdjacentIdenticalDigits(): Boolean =
            ((this[0] == this[1] && this[1] != this[2])
                    || (this[0] != this[1] && this[1] == this[2] && this[2] != this[3])
                    || (this[1] != this[2] && this[2] == this[3] && this[3] != this[4])
                    || (this[2] != this[3] && this[3] == this[4] && this[4] != this[5])
                    || (this[3] != this[4] && this[4] == this[5]))


    private fun String.hasAtLeastTwoAdjacentIdenticalDigits() =
            this[0] == this[1] || this[1] == this[2] || this[2] == this[3] || this[3] == this[4] || this[4] == this[5]
}

