package main

import (
	"github.com/chrisemerson/AdventOfCode/2018-Go/day1"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day2"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day3"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day4"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day5"
	"os"
	"strings"
)

func main() {
	switch strings.Join(os.Args[1:3], "_") {
	case "d1_p1":
		day1.Part1()
	case "d1_p2":
		day1.Part2()

	case "d2_p1":
		day2.Part1()
	case "d2_p2":
		day2.Part2()

	case "d3_p1":
		day3.Part1()
	case "d3_p2":
		day3.Part2()

	case "d4_p1":
		day4.Part1()
	case "d4_p2":
		day4.Part2()

	case "d5_p1":
		day5.Part1()
	case "d5_p2":
		day5.Part2()
	}
}
