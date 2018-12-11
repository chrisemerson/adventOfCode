package day8

import (
	"fmt"
	"strconv"
	"strings"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
)

func Part1() {
	data := strings.Split(util.GetInputAsString("day8/input.txt"), " ")
	data = []string{"2", "3", "0", "3", "10", "11", "12", "1", "1", "0", "1", "99", "2", "1", "1", "2"}

	nodes := returnNodes(data, 1)

	fmt.Println(nodes)
}

func Part2() {
}

func returnNodes(data []string, nextLabel int) map[int]map[string][]int {
	pointer := 0
	nodes := make(map[int]map[string][]int, 0)

	for pointer < len(data) {
		numChildren, _ := strconv.Atoi(data[0])
		numMetadata, _ := strconv.Atoi(data[1])
		children := []int{}
		metadata := []int{}

		if numChildren == 0 {
			for metaDataPointer := 2; metaDataPointer < numMetadata + 2; metaDataPointer++ {
				thisMetaData, _ := strconv.Atoi(data[metaDataPointer])
				metadata = append(metadata, thisMetaData)
			}

			pointer += numMetadata + 2
		} else {
			childNodes := returnNodes(data[pointer + 2:], nextLabel + 1)

			for idx, childNode := range childNodes {
				pointer += len(childNode["metadata"]) + 2
				children = append(children, idx)

				nodes[idx] = childNode
			}

			for metaDataPointer := pointer + 2; metaDataPointer < numMetadata + 2; metaDataPointer++ {
				thisMetaData, _ := strconv.Atoi(data[metaDataPointer])
				metadata = append(metadata, thisMetaData)
			}

			pointer += numMetadata
		}

		nodes[nextLabel] = make(map[string][]int, 0)
		nodes[nextLabel]["children"] = children
		nodes[nextLabel]["metadata"] = metadata

		nextLabel += 1
	}

	return nodes
}