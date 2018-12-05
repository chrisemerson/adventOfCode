package day3

import (
	"fmt"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
	"regexp"
	"strconv"
)

func Part1() {
	fabric := makeClaims()
	overlappingCells := 0

	for _, col := range fabric {
		for _, cell := range col {
			if len(cell) >= 2 {
				overlappingCells += 1
			}
		}
	}

	fmt.Print("Overlapping cells: ")
	fmt.Println(overlappingCells)
}

func Part2() {
	fabric := makeClaims()
	ids := make(map[string]bool)
	idsThatOverlap := make(map[string]bool)

	for _, col := range fabric {
		for _, cell := range col {
			for _, id := range cell {
				ids[id] = true
			}

			if len(cell) >= 2 {
				for _, id := range cell {
					idsThatOverlap[id] = true
				}
			}
		}
	}

	for id := range ids {
		if _, ok := idsThatOverlap[id]; !ok {
			fmt.Print("Non overlapping claim: ")
			fmt.Println(id)

			return
		}
	}
}

func makeClaims () [1000][1000][]string {
	input := util.GetInputAsArrayTrimmed("day3/input.txt")
	fabric := [1000][1000][]string{}

	r := regexp.MustCompile(`#(\d+) @ (\d+),(\d+): (\d+)x(\d+)`)

	for _, claim := range input {
		matches := r.FindStringSubmatch(claim)

		id := matches[1]
		x, _ := strconv.Atoi(matches[2])
		y, _ := strconv.Atoi(matches[3])
		w, _ := strconv.Atoi(matches[4])
		h, _ := strconv.Atoi(matches[5])

		for i := x; i < (x + w); i++ {
			for j := y; j < (y + h); j++ {
				fabric[i][j] = append(fabric[i][j], id)
			}
		}
	}

	return fabric
}