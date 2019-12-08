package uk.co.cemerson.aoc.Day8

import uk.co.cemerson.aoc.AOCDay

class Day8 : AOCDay {
    override fun part1() {
        val layers = getImageLayers()

        val layerCharFreqs = layers
                .map {
                    it
                            .groupingBy { it }
                            .eachCount()
                }

        val checksum = layerCharFreqs
                .filter { it['0'] == layerCharFreqs.map { it['0'] ?: 0 }.min() }
                .map { it['1'].toString().toInt() * it['2'].toString().toInt() }

        println("Checksum: " + checksum)
    }

    override fun part2() {
        val layers = getImageLayers()
        val finalImage = mutableListOf<Char>()

        for (pixel in 0 until (25 * 6)) {
            finalImage.add(layers
                    .map { it[pixel] }
                    .filter { it != '2' }
                    .first()
            )
        }

        for (line in finalImage.chunked(25)) {
            for (char in line) {
                print(if (char == '0') ' ' else '*')
            }

            println()
        }
    }

    private fun getImageLayers(): List<List<Char>> {
        val input = readFileContents("Day8/input.txt")
        val layers = input.toCharArray().toList().chunked(25 * 6)
        return layers
    }
}