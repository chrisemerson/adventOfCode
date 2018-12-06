package day6

import (
	"fmt"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
	"math"
	"strconv"
	"strings"
)

func Part1() {
	coordinates := util.GetInputAsArrayTrimmed("day6/input.txt")
	grid := [1000][1000]int{}
	infiniteregions := make(map[int]bool, 0)

	for x, col := range grid {
		for y := range col {
			closest := getClosestPoint(coordinates, x, y)
			grid[x][y] = closest

			if x == 0 || x == 999 || y == 0 || y == 999 {
				infiniteregions[closest] = true
			}
		}
	}

	areacounts := make(map[int]int, 0)

	for _, col := range grid {
		for _, closest := range col {
			if _, ok := infiniteregions[closest]; !ok {
				if _, ok := areacounts[closest]; !ok {
					areacounts[closest] = 1
				} else {
					areacounts[closest] += 1
				}
			}
		}
	}

	maxregion := 0

	for i, v := range areacounts {
		if v > maxregion && i != 0 {
			maxregion = v
		}
	}

	fmt.Print("Largest non-infinite region size: ")
	fmt.Println(maxregion)
}

func Part2() {
	coordinates := util.GetInputAsArrayTrimmed("day6/input.txt")
	regionsize := 0

	for x := 0; x < 1000; x++ {
		for y := 0; y < 1000; y++ {
			totalDist := getTotalManhattanDistance(coordinates, x, y)

			if totalDist < 10000 {
				regionsize += 1
			}
		}
	}

	fmt.Print("Size of region where total manhattan distance is < 10000: ")
	fmt.Println(regionsize)
}

func getClosestPoint(coordinates []string, x int, y int) int {
	mindist := -1
	minid := 0
	mindistcount := 0

	for i, v := range coordinates {
		coords := strings.Split(v, ", ")

		thisX, _ := strconv.Atoi(coords[0])
		thisY, _ := strconv.Atoi(coords[1])

		if manhattenDistance(thisX, thisY, x, y) == mindist {
			mindistcount += 1
		}

		if mindist == -1 || manhattenDistance(thisX, thisY, x, y) < mindist {
			mindist = manhattenDistance(thisX, thisY, x, y)
			minid = i
			mindistcount = 1
		}
	}

	if mindistcount == 1 {
		return minid
	} else {
		return 0
	}
}

func getTotalManhattanDistance(coordinates []string, x int, y int) int {
	totaldistance := 0

	for _, v := range coordinates {
		coords := strings.Split(v, ", ")

		thisX, _ := strconv.Atoi(coords[0])
		thisY, _ := strconv.Atoi(coords[1])

		totaldistance += manhattenDistance(thisX, thisY, x, y)
	}

	return totaldistance
}

func manhattenDistance(x1 int, y1 int, x2 int, y2 int) int {
	return int(math.Abs(float64(x1-x2)) + math.Abs(float64(y1-y2)))
}
