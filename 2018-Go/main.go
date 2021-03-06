package main

import (
	"github.com/chrisemerson/AdventOfCode/2018-Go/day1"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day10"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day11"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day12"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day13"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day14"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day15"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day16"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day17"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day18"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day19"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day2"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day3"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day4"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day5"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day6"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day7"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day8"
	"github.com/chrisemerson/AdventOfCode/2018-Go/day9"
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

	case "d6_p1":
		day6.Part1()
	case "d6_p2":
		day6.Part2()

	case "d7_p1":
		day7.Part1()
	case "d7_p2":
		day7.Part2()

	case "d8_p1":
		day8.Part1()
	case "d8_p2":
		day8.Part2()

	case "d9_p1":
		day9.Part1()
	case "d9_p2":
		day9.Part2()

	case "d10_p1":
		day10.Part1()
	case "d10_p2":
		day10.Part2()

	case "d11_p1":
		day11.Part1()
	case "d11_p2":
		day11.Part2()

	case "d12_p1":
		day12.Part1()
	case "d12_p2":
		day12.Part2()

	case "d13_p1":
		day13.Part1()
	case "d13_p2":
		day13.Part2()

	case "d14_p1":
		day14.Part1()
	case "d14_p2":
		day14.Part2()

	case "d15_p1":
		day15.Part1()
	case "d15_p2":
		day15.Part2()

	case "d16_p1":
		day16.Part1()
	case "d16_p2":
		day16.Part2()

	case "d17_p1":
		day17.Part1()
	case "d17_p2":
		day17.Part2()

	case "d18_p1":
		day18.Part1()
	case "d18_p2":
		day18.Part2()

	case "d19_p1":
		day19.Part1()
	case "d19_p2":
		day19.Part2()
	}
}
