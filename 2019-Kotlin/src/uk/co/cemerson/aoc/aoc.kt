package uk.co.cemerson.aoc

import uk.co.cemerson.aoc.Day1.Day1
import uk.co.cemerson.aoc.Day2.Day2

fun main(args: Array<String>) {
    if (args.size != 2) {
        println("Please provide a day number and a part number as a command line argument")
        return
    }

    val dayClass: AOCDay

    when (args[0]) {
        "1" -> dayClass = Day1()
        "2" -> dayClass = Day2()
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
