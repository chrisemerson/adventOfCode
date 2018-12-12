package day12

import (
	"fmt"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
	"math"
	"regexp"
)

func Part1() {
	plantState, transformations := getInput()

	printPlantState(plantState)
	fmt.Println("")

	for i := 0; i < 20; i++ {
		plantState = getNextPlantState(plantState, transformations)
		printPlantState(plantState)

		fmt.Print("Score: ")
		fmt.Println(getPlantStateSum(plantState))
	}
}

func Part2() {
	//After 1000 iterations, score increases by 36 per iteration. 35458 is the score at 1000.

	fmt.Print("Score after 50,000,000,000 iterations: ")
	fmt.Println(((50000000000 - 1000) * 36) + 35458)
}

func getInput() (map[int]string, map[string]string) {
	input := util.GetInputAsArrayTrimmed("day12/input.txt")

	plantState := make(map[int]string, 0)
	transformations := make(map[string]string, 0)

	initialStateRegex := regexp.MustCompile(`initial state: ([.#]+)`)
	transformRegex := regexp.MustCompile(`([.#]{5})\s=>\s([.#])`)

	plantState[-4] = "."
	plantState[-3] = "."
	plantState[-2] = "."
	plantState[-1] = "."

	for _, line := range input {
		if initialStateRegex.MatchString(line) {
			isMatches := initialStateRegex.FindStringSubmatch(line)

			for idx, char := range isMatches[1] {
				plantState[idx] = string(char)
			}
		}

		if transformRegex.MatchString(line) {
			transformMatches := transformRegex.FindStringSubmatch(line)

			transformations[transformMatches[1]] = transformMatches[2]
		}
	}

	return plantState, transformations
}

func getNextPlantState(plantState map[int]string, transformations map[string]string) map[int]string {
	lowestIdx := len(plantState)
	highestIdx := 0
	newPlantState := make(map[int]string, 0)

	for idx, val := range plantState {
		if val == "#" {
			if idx < lowestIdx {
				lowestIdx = idx
			}

			if idx > highestIdx {
				highestIdx = idx
			}
		}
	}

	for i := lowestIdx - 2; i <= highestIdx+2; i++ {
		currentState := ""

		for j := i - lowestIdx; j < 2; j++ {
			currentState += "."
		}

		for j := math.Max(float64(i-2), float64(lowestIdx)); j <= math.Min(float64(i+2), float64(highestIdx)); j++ {
			currentState += plantState[int(j)]
		}

		for j := highestIdx - i; j < 2; j++ {
			currentState += "."
		}

		if _, ok := transformations[currentState]; ok {
			newPlantState[i] = transformations[currentState]
		}
	}

	return newPlantState
}

func printPlantState(plantState map[int]string) {
	lowestIdx := 0
	highestIdx := 0

	for idx := range plantState {
		if idx < lowestIdx {
			lowestIdx = idx
		}

		if idx > highestIdx {
			highestIdx = idx
		}
	}

	for i := lowestIdx; i <= highestIdx; i++ {
		fmt.Print(plantState[i])
	}

	fmt.Println("")
}

func getPlantStateSum(plantState map[int]string) int {
	sum := 0

	for idx, plant := range plantState {
		if plant == "#" {
			sum += idx
		}
	}

	return sum
}
