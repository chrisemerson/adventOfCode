package day17

import (
	"fmt"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
	"math"
	"regexp"
)

func Part1() {
	coreMap, minX, maxX := getCoreMap()

	wetTiles := 1
	retainedWaterTiles := 0
	wetTilesPrev := 0

	for wetTiles > wetTilesPrev {
		for y := 0; y <= len(coreMap); y++ {
			for x := minX - 2; x <= maxX+2; x++ {
				if (coreMap[y][x] == "+" || coreMap[y][x] == "|") && coreMap[y+1][x] == "." {
					coreMap[y+1][x] = "|"
				}

				if coreMap[y][x] == "|" && (coreMap[y+1][x] == "#" || coreMap[y+1][x] == "~") {
					stopLeft := false
					hitLeftBarrier := false

					stopRight := false
					hitRightBarrier := false

					lx := x
					rx := x

					for ; !stopLeft && lx >= minX-2; lx-- {
						if coreMap[y][lx] == "." && coreMap[y+1][lx+1] != "|" {
							coreMap[y][lx] = "|"
						}

						if coreMap[y][lx] == "#" {
							hitLeftBarrier = true
							stopLeft = true
						}

						if coreMap[y+1][lx] == "." {
							stopLeft = true
						}
					}

					for ; !stopRight && rx <= maxX+2; rx++ {
						if coreMap[y][rx] == "." && coreMap[y+1][rx-1] != "|" {
							coreMap[y][rx] = "|"
						}

						if coreMap[y][rx] == "#" {
							hitRightBarrier = true
							stopRight = true
						}

						if coreMap[y+1][rx] == "." {
							stopRight = true
						}
					}

					if hitLeftBarrier && hitRightBarrier {
						for cx := lx + 2; cx < rx-1; cx++ {
							coreMap[y][cx] = "~"
						}
					}
				}
			}
		}

		wetTilesPrev = wetTiles
		wetTiles = 0
		retainedWaterTiles = 0

		for y := 3; y <= len(coreMap)-2; y++ {
			for x := minX - 2; x <= maxX+2; x++ {
				if coreMap[y][x] == "|" || coreMap[y][x] == "~" {
					wetTiles += 1
				}

				if coreMap[y][x] == "~" {
					retainedWaterTiles += 1
				}
			}
		}
	}

	fmt.Print("Number of wet tiles is ")
	fmt.Println(wetTiles)
	fmt.Print("Water retained is ")
	fmt.Println(retainedWaterTiles)
}

func Part2() {
	Part1()
}

func getCoreMap() (map[int]map[int]string, int, int) {
	input := util.GetInputAsArrayTrimmed("day17/input.txt")

	seamRegexp := regexp.MustCompile(`(x|y)=(\d+), (x|y)=(\d+)..(\d+)`)

	minX := 500
	maxX := 500
	maxY := 0

	for _, line := range input {
		matches := seamRegexp.FindStringSubmatch(line)

		if matches[1] == "x" {
			if util.Atoi(matches[2]) < minX {
				minX = util.Atoi(matches[2])
			}

			if util.Atoi(matches[2]) > maxX {
				maxX = util.Atoi(matches[2])
			}

			if util.Atoi(matches[4]) > maxY {
				maxY = util.Atoi(matches[4])
			}

			if util.Atoi(matches[5]) > maxY {
				maxY = util.Atoi(matches[5])
			}
		} else {
			if util.Atoi(matches[2]) > maxY {
				maxY = util.Atoi(matches[2])
			}

			if util.Atoi(matches[4]) > maxY {
				maxY = util.Atoi(matches[4])
			}

			if util.Atoi(matches[5]) > maxY {
				maxY = util.Atoi(matches[5])
			}

			if util.Atoi(matches[4]) > maxY {
				maxY = util.Atoi(matches[4])
			}

			if util.Atoi(matches[5]) > maxY {
				maxY = util.Atoi(matches[5])
			}
		}
	}

	coreMap := make(map[int]map[int]string, 0)

	for y := 0; y <= maxY+1; y++ {
		coreMap[y] = make(map[int]string, 0)

		for x := minX - 2; x <= maxX+2; x++ {
			coreMap[y][x] = "."
		}
	}

	coreMap[0][500] = "+"

	for _, line := range input {
		matches := seamRegexp.FindStringSubmatch(line)

		if matches[1] == "x" {
			xValue := util.Atoi(matches[2])

			yValue1 := util.Atoi(matches[4])
			yValue2 := util.Atoi(matches[5])

			for y := int(math.Min(float64(yValue1), float64(yValue2))); y <= int(math.Max(float64(yValue1), float64(yValue2))); y++ {
				coreMap[y][xValue] = "#"
			}
		} else {
			yValue := util.Atoi(matches[2])

			xValue1 := util.Atoi(matches[4])
			xValue2 := util.Atoi(matches[5])

			for x := int(math.Min(float64(xValue1), float64(xValue2))); x <= int(math.Max(float64(xValue1), float64(xValue2))); x++ {
				coreMap[yValue][x] = "#"
			}
		}
	}

	return coreMap, minX, maxX
}

func drawMap(coreMap map[int]map[int]string, xMin int, xMax int) {
	for y := 0; y < len(coreMap); y++ {
		for x := xMin - 1; x <= xMax+1; x++ {
			fmt.Print(coreMap[y][x])
		}

		fmt.Println("")
	}
}
