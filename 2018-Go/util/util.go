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