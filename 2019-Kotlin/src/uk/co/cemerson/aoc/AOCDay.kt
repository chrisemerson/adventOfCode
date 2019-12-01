package uk.co.cemerson.aoc

import java.io.File

interface AOCDay {
    fun part1()
    fun part2()

    fun readFileLines(filename: String): List<String> {
        return File("src/uk/co/cemerson/aoc/" + filename).readLines()
    }
}
