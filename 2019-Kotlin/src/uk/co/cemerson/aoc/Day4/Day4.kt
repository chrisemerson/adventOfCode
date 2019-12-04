package uk.co.cemerson.aoc.Day4

import uk.co.cemerson.aoc.AOCDay

class Day4 : AOCDay {
    override fun part1() {
        val passwords = (172930..683082)
                .map { it.toString() }
                .filter { it[0] == it[1] || it[1] == it[2] || it[2] == it[3] || it[3] == it[4] || it[4] == it[5] }
                .filter { it[0] <= it[1] && it[1] <= it[2] && it[2] <= it[3] && it[3] <= it[4] && it[4] <= it[5] }
                .count()

        println("Number of passwords: " + passwords)
    }

    override fun part2() {
        val passwords = (172930..683082)
                .map { it.toString() }
                .filter {
                    (it[0] == it[1] && it [1] != it[2])
                            || (it[0] != it[1] && it[1] == it[2] && it [2] != it[3])
                            || (it[1] != it[2] && it[2] == it[3] && it [3] != it[4])
                            || (it[2] != it[3] && it[3] == it[4] && it [4] != it[5])
                            || (it[3] != it[4] && it[4] == it[5])
                }
                .filter { it[0] <= it[1] && it[1] <= it[2] && it[2] <= it[3] && it[3] <= it[4] && it[4] <= it[5] }
                .count()

        println("Number of passwords: " + passwords)
    }
}

