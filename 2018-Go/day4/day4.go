package day4

import (
	"fmt"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
	"regexp"
	"sort"
	"strconv"
)

func Part1() {
	timetable := getTimetable()
	minsSleepingPerGuard := make(map[string]int)

	for _, guardMap := range timetable {
		for guard, asleep := range guardMap {
			minsSleepingPerGuard[guard] += asleep
		}
	}

	max := 0
	var maxGuard string

	for guard, minsasleep := range minsSleepingPerGuard {
		if minsasleep > max {
			max = minsasleep
			maxGuard = guard
		}
	}

	fmt.Print("Guard asleep the most: ")
	fmt.Println(maxGuard)

	maxGuardAsleepByMins := [60]int{}

	for min, guardMap := range timetable {
		maxGuardAsleepByMins[min] += guardMap[maxGuard]
	}

	maxSleep := 0
	var minMaxSleep int

	for min, sleep := range maxGuardAsleepByMins {
		if sleep > maxSleep {
			maxSleep = sleep
			minMaxSleep = min
		}
	}

	fmt.Print("Minute this guard is most asleep: ")
	fmt.Println(minMaxSleep)

	maxGuardInt, _ := strconv.Atoi(maxGuard)
	fmt.Print("Multiplied: ")
	fmt.Println(minMaxSleep * maxGuardInt)
}

func Part2() {
	timetable := getTimetable()

	maxSleep := 0
	var maxGuard string
	var maxMin int

	for min, guardMap := range timetable {
		for guard, asleep := range guardMap {
			if asleep > maxSleep {
				maxSleep = asleep
				maxGuard = guard
				maxMin = min
			}
		}
	}

	fmt.Print("Guard most frequently asleep on same minute: ")
	fmt.Println(maxGuard)
	fmt.Print("Minute that guard is most frequently asleep: ")
	fmt.Println(maxMin)

	maxGuardInt, _ := strconv.Atoi(maxGuard)

	fmt.Print("Multiplied: ")
	fmt.Println(maxGuardInt * maxMin)
}

func getTimetable() [60]map[string]int {
	input := util.GetInputAsArrayTrimmed("day4/input.txt")
	timetable := [60]map[string]int{}

	for i := 0; i < 60; i++ {
		timetable[i] = make(map[string]int)
	}

	sort.Strings(input)

	var curGuard string

	minsRegex := regexp.MustCompile(`\[\d{4}-\d{2}-\d{2} (\d{2}):(\d{2})\] (.*)`)
	instructionRegex := regexp.MustCompile(`Guard #(\d+) begins shift`)

	for _, v := range input {
		minsMatches := minsRegex.FindStringSubmatch(v)

		hours := minsMatches[1]
		mins := minsMatches[2]
		instruction := minsMatches[3]

		if instructionRegex.MatchString(instruction) {
			instructionMatches := instructionRegex.FindStringSubmatch(instruction)
			curGuard = instructionMatches[1]

			for i := 0; i < 60; i++ {
				if _, ok := timetable[i][curGuard]; !ok {
					timetable[i][curGuard] = 0
				}
			}
		}

		if hours == "00" {
			if instruction == "falls asleep" {
				for i, _ := strconv.Atoi(mins); i < 60; i++ {
					timetable[i][curGuard] += 1
				}
			}

			if instruction == "wakes up" {
				for i, _ := strconv.Atoi(mins); i < 60; i++ {
					timetable[i][curGuard] -= 1
				}
			}
		}
	}

	return timetable
}
