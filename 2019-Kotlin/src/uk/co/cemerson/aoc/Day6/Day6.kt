package uk.co.cemerson.aoc.Day6

import uk.co.cemerson.aoc.AOCDay

class Day6 : AOCDay {
    override fun part1() {
        val orbits = getOrbitsMap()
        var orbitCount = 0

        orbits.forEach {
            orbitCount += countOrbitsFromObject(orbits, it.key)
        }

        println(orbitCount)
    }

    override fun part2() {
        val orbits = getOrbitsMap()

        val youOrbits = getParentOrbits(orbits, "YOU")
        val sanOrbits = getParentOrbits(orbits, "SAN")
        val commonAncestor = getCommonAncestor(youOrbits, sanOrbits)

        //Take 4 from total - 1 for repeated ancestor, 1 for fencepost issue, 2 for YOU and SAN which aren't being transferred between
        val orbitHops = youOrbits.dropWhile { it != commonAncestor }.count() + sanOrbits.dropWhile { it != commonAncestor }.count() - 4

        println("Orbit Hops: " + orbitHops)
    }

    private fun countOrbitsFromObject(orbits: Map<String, String>, obj: String): Int =
            if (obj == "COM") 0 else countOrbitsFromObject(orbits, orbits.getOrDefault(obj, "COM")) + 1

    private fun getParentOrbits(orbits: Map<String, String>, obj: String): List<String> =
            if (obj == "COM") listOf("COM") else (getParentOrbits(orbits, orbits.getOrDefault(obj, "COM")).toMutableList() + listOf(obj))

    private fun getCommonAncestor(list1: List<String>, list2: List<String>): String? {
        list1.reversed().forEach { if (it in list2) return it }
        return null
    }

    private fun getOrbitsMap(): MutableMap<String, String> {
        val input = readFileLines("Day6/input.txt")
        val orbits = mutableMapOf<String, String>()

        input.forEach {
            val (orbitcentre, orbiter) = it.split(')')
            orbits.put(orbiter, orbitcentre)
        }

        return orbits
    }
}