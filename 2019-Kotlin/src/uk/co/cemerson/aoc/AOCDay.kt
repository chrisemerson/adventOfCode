package uk.co.cemerson.aoc

import java.io.File

interface AOCDay {
    fun part1()
    fun part2()

    fun readFileContents(filename: String): String {
        return File("src/uk/co/cemerson/aoc/" + filename).readText(Charsets.UTF_8).trim()
    }

    fun readFileLines(filename: String): List<String> {
        return File("src/uk/co/cemerson/aoc/" + filename).readLines()
    }

    fun readFileSplitByChar(filename: String, splitBy: Char): List<String> {
        return readFileContents(filename).split(splitBy)
    }
}
