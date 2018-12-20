package day16

import (
	"fmt"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
	"regexp"
)

func Part1() {
	inputMap := getInputMap()
	opcodeMap := []string{}

	numberOfInputThatBehaveLike3OrMoreInstructions := 0

	opcodeMap = append(opcodeMap, "addr")
	opcodeMap = append(opcodeMap, "addi")
	opcodeMap = append(opcodeMap, "mulr")
	opcodeMap = append(opcodeMap, "muli")
	opcodeMap = append(opcodeMap, "banr")
	opcodeMap = append(opcodeMap, "bani")
	opcodeMap = append(opcodeMap, "borr")
	opcodeMap = append(opcodeMap, "bori")
	opcodeMap = append(opcodeMap, "setr")
	opcodeMap = append(opcodeMap, "seti")
	opcodeMap = append(opcodeMap, "gtir")
	opcodeMap = append(opcodeMap, "gtri")
	opcodeMap = append(opcodeMap, "gtrr")
	opcodeMap = append(opcodeMap, "eqir")
	opcodeMap = append(opcodeMap, "eqri")
	opcodeMap = append(opcodeMap, "eqrr")

	for i := 0; i < len(inputMap); i++ {
		test := inputMap[i]
		instBehavesLike := 0

		for _, inst := range opcodeMap {
			if testInst(inst, test["inst"][1], test["inst"][2], test["inst"][3], test["before"], test["after"]) {
				instBehavesLike += 1
			}
		}

		if instBehavesLike >= 3 {
			numberOfInputThatBehaveLike3OrMoreInstructions += 1
		}
	}

	fmt.Print("Number of samples that behave like 3 or more opcodes: ")
	fmt.Println(numberOfInputThatBehaveLike3OrMoreInstructions)
}

func Part2() {
	inputMap := getInputMap()
	opcodeMap := make(map[int]map[string]bool, 0)
	opCodes := make(map[int]string, 0)

	for i := 0; i < 16; i++ {
		opcodeMap[i] = make(map[string]bool, 0)

		opcodeMap[i]["addr"] = true
		opcodeMap[i]["addi"] = true
		opcodeMap[i]["mulr"] = true
		opcodeMap[i]["muli"] = true
		opcodeMap[i]["banr"] = true
		opcodeMap[i]["bani"] = true
		opcodeMap[i]["borr"] = true
		opcodeMap[i]["bori"] = true
		opcodeMap[i]["setr"] = true
		opcodeMap[i]["gtir"] = true
		opcodeMap[i]["gtri"] = true
		opcodeMap[i]["gtrr"] = true
		opcodeMap[i]["eqir"] = true
		opcodeMap[i]["eqri"] = true
		opcodeMap[i]["eqrr"] = true
	}

	for _, test := range inputMap {
		opcodeBeingTested := test["inst"][0]

		for inst := range opcodeMap[opcodeBeingTested] {
			if !testInst(inst, test["inst"][1], test["inst"][2], test["inst"][3], test["before"], test["after"]) {
				opcodeMap[opcodeBeingTested][inst] = false
			}
		}
	}

	allOpcodesFound := false

	for !allOpcodesFound {
		for i := 0; i < 16; i++ {
			possibilities := 0

			for _, true := range opcodeMap[i] {
				if true {
					possibilities += 1
				}
			}

			if possibilities == 1 {
				for inst, true := range opcodeMap[i] {
					if true {
						opCodes[i] = inst

						for j := 0; j < 16; j++ {
							opcodeMap[j][inst] = false
						}
					}
				}
			}
		}

		allOpcodesFound = true

		for i := 0; i < 16; i++ {
			for _, true := range opcodeMap[i] {
				if true {
					allOpcodesFound = false
				}
			}
		}
	}

	program := util.GetInputAsArrayTrimmed("day16/inputpt2.txt")

	regexInst := regexp.MustCompile(`(\d+) (\d+) (\d+) (\d+)`)

	registers := []int{0, 0, 0, 0}

	for _, line := range program {
		instMatches := regexInst.FindStringSubmatch(line)
		registers = util.ExecOpcode(opCodes[util.Atoi(instMatches[1])], util.Atoi(instMatches[2]), util.Atoi(instMatches[3]), util.Atoi(instMatches[4]), registers)
	}

	fmt.Print("Final value of register 0: ")
	fmt.Println(registers[0])
}

func getInputMap () []map[string][]int {
	input := util.GetInputAsArrayTrimmed("day16/inputpt1.txt")
	inputMap := make([]map[string][]int, 0)

	regexBefore := regexp.MustCompile(`Before:\s+\[(\d+), (\d+), (\d+), (\d+)\]`)
	regexInst := regexp.MustCompile(`(\d+) (\d+) (\d+) (\d+)`)
	regexAfter := regexp.MustCompile(`After:\s+\[(\d+), (\d+), (\d+), (\d+)\]`)

	for i := 0; i < len(input); i += 3 {
		beforeMatches := regexBefore.FindStringSubmatch(input[i])
		instMatches := regexInst.FindStringSubmatch(input[i + 1])
		afterMatches := regexAfter.FindStringSubmatch(input[i + 2])

		example := make(map[string][]int, 0)

		beforeArray := []int{}

		beforeArray = append(beforeArray, util.Atoi(beforeMatches[1]))
		beforeArray = append(beforeArray, util.Atoi(beforeMatches[2]))
		beforeArray = append(beforeArray, util.Atoi(beforeMatches[3]))
		beforeArray = append(beforeArray, util.Atoi(beforeMatches[4]))

		instArray := []int{}

		instArray = append(instArray, util.Atoi(instMatches[1]))
		instArray = append(instArray, util.Atoi(instMatches[2]))
		instArray = append(instArray, util.Atoi(instMatches[3]))
		instArray = append(instArray, util.Atoi(instMatches[4]))

		afterArray := []int{}

		afterArray = append(afterArray, util.Atoi(afterMatches[1]))
		afterArray = append(afterArray, util.Atoi(afterMatches[2]))
		afterArray = append(afterArray, util.Atoi(afterMatches[3]))
		afterArray = append(afterArray, util.Atoi(afterMatches[4]))

		example["before"] = beforeArray
		example["inst"] = instArray
		example["after"] = afterArray

		inputMap = append(inputMap, example)
	}

	return inputMap
}

func testInst(inst string, a int, b int, c int, before []int, expectedAfter []int) bool {
	after := util.ExecOpcode(inst, a, b, c, before)

	return expectedAfter[0] == after[0] && expectedAfter[1] == after[1] && expectedAfter[2] == after[2] && expectedAfter[3] == after[3]
}
