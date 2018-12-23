package day19

import (
	"fmt"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
	"regexp"
)

func Part1() {
	instructions, ipRegister := getInput()

	registers := []int{0, 0, 0, 0, 0, 0}
	ip := 0

	for ip >= 0 && ip < len(instructions) {
		registers[ipRegister] = ip
		inst := instructions[ip]

		registers = util.ExecOpcode(inst["instruction"], util.Atoi(inst["a"]), util.Atoi(inst["b"]), util.Atoi(inst["c"]), registers)
		ip = registers[ipRegister]

		ip++
	}


	fmt.Println(registers)
}

func Part2() {
	//Manual 'de-compilation' of assembler back into native Go code, followed by optimisation
	//Turned out to be an algorithm to sum the factors of a given number - changing register 0 in the original program changed the number to 10551398
	//Was a couple of nested loops that tested i * j rather than just checking divisibility, optimised below...
	//Factors were being summed into register 0

	sum := 0

	for i := 1; i <= 10551398; i++ {
		if 10551398 % i == 0 {
			sum += i
		}
	}

	fmt.Print("Register 0 at end of program: ")
	fmt.Println(sum)
}

func getInput () ([]map[string]string, int) {
	ipRegex := regexp.MustCompile(`#ip (\d+)`)
	instRegex := regexp.MustCompile(`([a-z]{4}) (\d+) (\d+) (\d+)`)

	input := util.GetInputAsArrayTrimmed("day19/input.txt")
	instructionPointer := 0
	instructions := make([]map[string]string, 0)


	for _, line := range input {
		if ipRegex.MatchString(line) {
			matches := ipRegex.FindStringSubmatch(line)
			instructionPointer = util.Atoi(matches[1])
		}

		if instRegex.MatchString(line) {
			matches := instRegex.FindStringSubmatch(line)
			instruction := make(map[string]string, 0)

			instruction["instruction"] = matches[1]
			instruction["a"] = matches[2]
			instruction["b"] = matches[3]
			instruction["c"] = matches[4]

			instructions = append(instructions, instruction)
		}
	}

	return instructions, instructionPointer
}