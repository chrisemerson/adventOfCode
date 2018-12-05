package day5

import (
	"fmt"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
	"strings"
)

func Part1() {
	polymer := strings.TrimSpace(util.GetInputAsString("day5/input.txt"))

	polymer = reactPolymer(polymer)

	fmt.Print("Polymer length: ")
	fmt.Println(len(polymer))
}

func Part2() {
	polymer := strings.TrimSpace(util.GetInputAsString("day5/input.txt"))

	var thisPolymer string
	shortestPolymer := 999999 // Bigger than pt1 solution

	for i := 1; i <= 26; i++ {
		strToReplace := string(rune('A' - 1 + i))

		thisPolymer = strings.Replace(polymer, strings.ToUpper(strToReplace), "", -1)
		thisPolymer = strings.Replace(thisPolymer, strings.ToLower(strToReplace), "", -1)

		thisPolymer = reactPolymer(thisPolymer)

		if len(thisPolymer) < shortestPolymer {
			shortestPolymer = len(thisPolymer)
		}
	}

	fmt.Print("Shortest polymer length: ")
	fmt.Println(shortestPolymer)
}

func reactPolymer(polymer string) string {
	keepReacting := true

	for keepReacting {
		newPolymer := takeOutSingleReaction(polymer)

		if len(newPolymer) == len(polymer) {
			keepReacting = false
		}

		polymer = newPolymer
	}

	return polymer
}

func takeOutSingleReaction(polymer string) string {
	for i, char := range polymer {
		if i+1 < len(polymer) {
			unit := string(char)
			nextUnit := string(polymer[i+1])

			if strings.ToUpper(unit) == strings.ToUpper(nextUnit) && ((strings.ToUpper(unit) == unit && strings.ToLower(nextUnit) == nextUnit) || (strings.ToLower(unit) == unit && strings.ToUpper(nextUnit) == nextUnit)) {
				return strings.Join([]string{polymer[:i], polymer[i+2:]}, "")
			}
		}
	}

	return polymer
}
