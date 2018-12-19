package day18

import (
	"fmt"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
)

func Part1() {
	treeMap := getInput()

	runMinutes(treeMap, 10)
}

func Part2() {
	treeMap := getInput()

	runMinutes(treeMap, 1000000000)
}

func runMinutes(treeMap map[int]map[int]string, minutes int) {
	i := 0

	// Get well into the cycles
	for ; i < 1000; i++ {
		treeMap = getNextState(treeMap)
	}

	for ; i < (minutes - 1000); i += 27 {
		//State doesn't change every 27 iterations
	}

	for ; i < minutes; i++ {
		treeMap = getNextState(treeMap)
	}

	noLumberyards := 0
	noTrees := 0

	for y := 0; y < len(treeMap); y++ {
		for x := 0; x < len(treeMap[y]); x++ {
			if treeMap[y][x] == "#" {
				noLumberyards++
			}

			if treeMap[y][x] == "|" {
				noTrees++
			}
		}
	}

	fmt.Print("Total of ")
	fmt.Print(noTrees)
	fmt.Print(" trees and ")
	fmt.Print(noLumberyards)
	fmt.Print(" lumberyards for a total resource value of: ")
	fmt.Println(noTrees * noLumberyards)
}

func getNextState(input map[int]map[int]string) map[int]map[int]string {
	output := make(map[int]map[int]string, 0)

	for y := 0; y < len(input); y++ {
		output[y] = make(map[int]string, 0)

		for x := 0; x < len(input[y]); x++ {
			output[y][x] = input[y][x]

			switch input[y][x] {
			case ".":
				if countNeighbours(input, y, x, "|") >= 3 {
					output[y][x] = "|"
				}

			case "|":
				if countNeighbours(input, y, x, "#") >= 3 {
					output[y][x] = "#"
				}

			case "#":
				if countNeighbours(input, y, x, "#") < 1 || countNeighbours(input, y, x, "|") < 1 {
					output[y][x] = "."
				}
			}
		}
	}

	return output
}

func getInput() map[int]map[int]string {
	input := util.GetInputAsArrayTrimmed("day18/input.txt")

	treeMap := make(map[int]map[int]string, 0)

	for y, line := range input {
		treeMap[y] = make(map[int]string, 0)

		for x, char := range line {
			treeMap[y][x] = string(char)
		}
	}

	return treeMap
}

func countNeighbours(input map[int]map[int]string, y int, x int, square string) int {
	number := 0

	if y > 0 {
		if x > 0 {
			if input[y-1][x-1] == square {
				number++
			}
		}

		if input[y-1][x] == square {
			number++
		}

		if x < len(input[y-1])-1 {
			if input[y-1][x+1] == square {
				number++
			}
		}
	}

	if x > 0 {
		if input[y][x-1] == square {
			number++
		}
	}

	if x < len(input[y])-1 {
		if input[y][x+1] == square {
			number++
		}
	}

	if y < len(input)-1 {
		if x > 0 {
			if input[y+1][x-1] == square {
				number++
			}
		}

		if input[y+1][x] == square {
			number++
		}

		if x < len(input[y+1])-1 {
			if input[y+1][x+1] == square {
				number++
			}
		}
	}

	return number
}

func drawMap(treeMap map[int]map[int]string) {
	for y := 0; y < len(treeMap); y++ {
		for x := 0; x < len(treeMap[y]); x++ {
			fmt.Print(treeMap[y][x])
		}

		fmt.Println("")
	}

	fmt.Println("")
}
