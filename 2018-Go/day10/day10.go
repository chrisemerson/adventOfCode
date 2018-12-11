package day10

import (
	"fmt"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
	"regexp"
	"strconv"
)

func Part1() {
	particles := getParticles()

	start := 0
	tries := 100000
	step := 1

	initialMinX, initialMaxX, initialMinY, initialMaxY := findBoundingBox(particles, 0)
	smallestArea := (initialMaxX - initialMinX) * (initialMaxY - initialMinY)
	smallestAreaTime := 0

	for time := start; time <= tries; time += step {
		minX, maxX, minY, maxY := findBoundingBox(particles, time)

		if (maxX - minX) * (maxY - minY) < smallestArea {
			smallestArea = (maxX - minX) * (maxY - minY)
			smallestAreaTime = time
		}
	}

	fmt.Print("Smallest at time ")
	fmt.Println(smallestAreaTime)

	displayParticlesAtTime(particles, smallestAreaTime)
}

func Part2() {
	Part1()
}

func getParticles() []map[string]int {
	input := util.GetInputAsArrayTrimmed("day10/input.txt")
	particles := make([]map[string]int, 0)

	particleRegex := regexp.MustCompile(`position=\<\s*(-?\d+)\s*,\s*(-?\d+)\s*\> velocity=\<\s*(-?\d+)\s*,\s*(-?\d+)\s*\>`)

	for _, line := range input {
		matches := particleRegex.FindStringSubmatch(line)

		particle := make(map[string]int, 0)
		particle["x"], _ = strconv.Atoi(matches[1])
		particle["y"], _ = strconv.Atoi(matches[2])
		particle["dx"], _ = strconv.Atoi(matches[3])
		particle["dy"], _ = strconv.Atoi(matches[4])

		particles = append(particles, particle)
	}

	return particles
}

func displayParticlesAtTime(particles []map[string]int, time int) {
    minX, maxX, minY, maxY := findBoundingBox(particles, time)

    grid := make(map[int]map[int]string, 0)

	for y := minY; y <= maxY; y++ {
		grid[y] = make(map[int]string, 0)

		for x := minX; x <= maxX; x++ {
			grid[y][x] = "."
		}
	}

    for _, particle := range particles {
		particleX := particle["x"] + (time * particle["dx"])
		particleY := particle["y"] + (time * particle["dy"])

		grid[particleY][particleX] = "#"
	}

	for y := minY; y <= maxY; y++ {
		for x := minX; x <= maxX; x++ {
			fmt.Print(grid[y][x])
		}

		fmt.Println("")
	}
}

func findBoundingBox(particles []map[string]int, time int) (int, int, int, int) {
	minX := particles[0]["x"] + (time * particles[0]["dx"])
	maxX := particles[0]["x"] + (time * particles[0]["dx"])
	minY := particles[0]["y"] + (time * particles[0]["dy"])
	maxY := particles[0]["y"] + (time * particles[0]["dy"])

	for _, particle := range particles {
		particleX := particle["x"] + (time * particle["dx"])
		particleY := particle["y"] + (time * particle["dy"])

		if particleX < minX {
			minX = particleX
		}

		if particleX > maxX {
			maxX = particleX
		}

		if particleY < minY {
			minY = particleY
		}

		if particleY > maxY {
			maxY = particleY
		}
	}

	return minX, maxX, minY, maxY
}