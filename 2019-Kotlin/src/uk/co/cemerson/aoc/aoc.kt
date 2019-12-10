package uk.co.cemerson.aoc

import uk.co.cemerson.aoc.Day1.Day1
import uk.co.cemerson.aoc.Day10.Day10
import uk.co.cemerson.aoc.Day11.Day11
import uk.co.cemerson.aoc.Day12.Day12
import uk.co.cemerson.aoc.Day13.Day13
import uk.co.cemerson.aoc.Day14.Day14
import uk.co.cemerson.aoc.Day15.Day15
import uk.co.cemerson.aoc.Day16.Day16
import uk.co.cemerson.aoc.Day17.Day17
import uk.co.cemerson.aoc.Day18.Day18
import uk.co.cemerson.aoc.Day19.Day19
import uk.co.cemerson.aoc.Day2.Day2
import uk.co.cemerson.aoc.Day20.Day20
import uk.co.cemerson.aoc.Day21.Day21
import uk.co.cemerson.aoc.Day22.Day22
import uk.co.cemerson.aoc.Day23.Day23
import uk.co.cemerson.aoc.Day24.Day24
import uk.co.cemerson.aoc.Day25.Day25
import uk.co.cemerson.aoc.Day3.Day3
import uk.co.cemerson.aoc.Day4.Day4
import uk.co.cemerson.aoc.Day5.Day5
import uk.co.cemerson.aoc.Day6.Day6
import uk.co.cemerson.aoc.Day7.Day7
import uk.co.cemerson.aoc.Day8.Day8
import uk.co.cemerson.aoc.Day9.Day9

fun main(args: Array<String>) {
    if (args.size != 2) {
        println("Please provide a day number and a part number as a command line argument")
        return
    }

    val dayClass: AOCDay

    when (args[0]) {
        "1" -> dayClass = Day1()
        "2" -> dayClass = Day2()
        "3" -> dayClass = Day3()
        "4" -> dayClass = Day4()
        "5" -> dayClass = Day5()
        "6" -> dayClass = Day6()
        "7" -> dayClass = Day7()
        "8" -> dayClass = Day8()
        "9" -> dayClass = Day9()
        "10" -> dayClass = Day10()
        "11" -> dayClass = Day11()
        "12" -> dayClass = Day12()
        "13" -> dayClass = Day13()
        "14" -> dayClass = Day14()
        "15" -> dayClass = Day15()
        "16" -> dayClass = Day16()
        "17" -> dayClass = Day17()
        "18" -> dayClass = Day18()
        "19" -> dayClass = Day19()
        "20" -> dayClass = Day20()
        "21" -> dayClass = Day21()
        "22" -> dayClass = Day22()
        "23" -> dayClass = Day23()
        "24" -> dayClass = Day24()
        "25" -> dayClass = Day25()

        else -> {
            println("Invalid Day specified")
            return
        }
    }

    if (args[1] == "1") {
        dayClass.part1()
    } else if (args[1] == "2") {
        dayClass.part2()
    } else {
        println("2nd Argument must be a 1 or 2")
        return
    }
}
