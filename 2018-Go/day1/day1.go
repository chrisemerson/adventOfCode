package day1

import (
	"fmt"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
	"strconv"
)

func Part1() {
	input := util.GetInputAsArrayTrimmed("day1/input.txt")
	freq := 0

	for _, v := range input {
		i, err := strconv.Atoi(v)

		if err != nil {
			panic(err)
		}

		freq += i
	}

	fmt.Print("Final frequency: ")
	fmt.Println(freq)
}

func Part2() {
	input := util.GetInputAsArrayTrimmed("day1/input.txt")
	freq := 0
	freqs := make(map[int]bool)

	freqs[0] = true

	for true {
		for _, v := range input {
			i, err := strconv.Atoi(v)

			if err != nil {
				panic(err)
			}

			freq += i

			if freqs[freq] {
				fmt.Print("First frequency reached twice: ")
				fmt.Println(freq)

				return
			}

			freqs[freq] = true
		}
	}
}
