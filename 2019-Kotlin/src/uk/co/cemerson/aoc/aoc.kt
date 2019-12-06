package uk.co.cemerson.aoc

import uk.co.cemerson.aoc.Day1.Day1
import uk.co.cemerson.aoc.Day2.Day2
import uk.co.cemerson.aoc.Day3.Day3
import uk.co.cemerson.aoc.Day4.Day4
import uk.co.cemerson.aoc.Day5.Day5
import uk.co.cemerson.aoc.Day6.Day6

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
