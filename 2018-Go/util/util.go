package util

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

func GetInputAsString(file string) string {
	return getInput(file)
}

func GetInputAsArray(file string) []string {
	return strings.Split(getInput(file), "\n")
}

func GetInputAsArrayTrimmed(file string) []string {
	input := GetInputAsArray(file)

	result := make([]string, 0)

	for _, v := range input {
		trimmed := strings.TrimSpace(v)

		if trimmed != "" {
			result = append(result, trimmed)
		}
	}

	return result
}

func getInput(file string) string {
	dir, err := filepath.Abs(filepath.Dir(os.Args[0]))

	if err != nil {
		panic(err)
	}

	dat, err := ioutil.ReadFile(strings.Join([]string{dir, file}, "/"))

	if err != nil {
		panic(err)
	}

	return string(dat)
}

func Atoi(str string) int {
	number, _ := strconv.Atoi(str)
	return number
}

func ExecOpcode(inst string, a int, b int, c int, before []int) []int {
	after := []int{}

	for i := 0; i < len(before); i++ {
		after = append(after, before[i])
	}

	switch inst {
	case "addr":
		after[c] = before[a] + before[b]

	case "addi":
		after[c] = before[a] + b

	case "mulr":
		after[c] = before[a] * before[b]

	case "muli":
		after[c] = before[a] * b

	case "banr":
		after[c] = before[a] & before[b]

	case "bani":
		after[c] = before[a] & b

	case "borr":
		after[c] = before[a] | before[b]

	case "bori":
		after[c] = before[a] | b

	case "setr":
		after[c] = before[a]

	case "seti":
		after[c] = a

	case "gtir":
		if a > before[b] {
			after[c] = 1
		} else {
			after[c] = 0
		}

	case "gtri":
		if before[a] > b {
			after[c] = 1
		} else {
			after[c] = 0
		}

	case "gtrr":
		if before[a] > before[b] {
			after[c] = 1
		} else {
			after[c] = 0
		}

	case "eqir":
		if a == before[b] {
			after[c] = 1
		} else {
			after[c] = 0
		}

	case "eqri":
		if before[a] == b {
			after[c] = 1
		} else {
			after[c] = 0
		}

	case "eqrr":
		if before[a] == before[b] {
			after[c] = 1
		} else {
			after[c] = 0
		}
	}

	return after
}
