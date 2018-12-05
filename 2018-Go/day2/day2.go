package day2

import (
	"fmt"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
	"strings"
)

func Part1() {
	input := util.GetInputAsArrayTrimmed("day2/input.txt")
	boxeswith2letters := 0
	boxeswith3letters := 0

	for _, v := range input {
		freqs := getCharFreqs(v)

		has2letters := false
		has3letters := false

		for _, n := range freqs {
			if n == 2 {
				has2letters = true
			}

			if n == 3 {
				has3letters = true
			}
		}

		if has2letters {
			boxeswith2letters += 1
		}

		if has3letters {
			boxeswith3letters += 1
		}
	}

	fmt.Print("Checksum: ")
	fmt.Println(boxeswith2letters * boxeswith3letters)
}

func Part2() {
	input := util.GetInputAsArrayTrimmed("day2/input.txt")

	for i := 0; i < len(input[0]); i++ {
		boxIDsWithCharRemoved := make([]string, 0)

		for _, boxid := range input {
			boxIDsWithCharRemoved = append(boxIDsWithCharRemoved, strings.Join([]string{boxid[:i], boxid[i+1:]}, ""))
		}

		encounteredBoxes := make(map[string]bool, 0)

		for _, modifiedBoxID := range boxIDsWithCharRemoved {
			if _, ok := encounteredBoxes[modifiedBoxID]; ok {
				fmt.Print("Common letters between similar box IDs: ")
				fmt.Println(modifiedBoxID)

				return
			} else {
				encounteredBoxes[modifiedBoxID] = true
			}
		}
	}
}

func getCharFreqs(input string) map[rune]int {
	freqs := make(map[rune]int, 0)

	for _, v := range input {
		if _, ok := freqs[v]; ok {
			freqs[v] += 1
		} else {
			freqs[v] = 1
		}
	}

	return freqs
}