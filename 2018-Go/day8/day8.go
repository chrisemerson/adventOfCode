package day8

import (
	"fmt"
	"strconv"
	"strings"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
)

func Part1() {
	nodes := getInput()

	fmt.Print(nodes)

	metaDataTotal := 0

	for _, node := range nodes {
		for _, metaData := range node["metadata"] {
			metaDataTotal += metaData
		}
	}

	fmt.Print("Metadata total: ")
	fmt.Println(metaDataTotal)
}

func Part2() {
}

func getInput() map[int]map[string][]int {
	data := strings.Split(util.GetInputAsString("day8/input.txt"), " ")
	//data = []string{"2", "3", "0", "3", "10", "11", "12", "1", "1", "0", "1", "99", "2", "1", "1", "2"}

	dataInt := []int{}

	for _, dataPoint := range data {
		dataPointInt, _ := strconv.Atoi(strings.TrimSpace(dataPoint))
		dataInt = append(dataInt, dataPointInt)
	}

	noChildren := dataInt[0]
	noMetadata := dataInt[1]

	nodes := make(map[int]map[string][]int, 0)
	node := make(map[string][]int, 0)

	label := 1

	childNodes, remainingData := returnNodes(dataInt[2:], noChildren, label + 1)

	alreadyParented := []int{}

	for _, childNode := range childNodes {
		alreadyParented = append(alreadyParented, childNode["children"]...)
	}

	for childLabel, childNode := range childNodes {
		alreadyHasParent := false

		for _, alreadyParentedItem := range alreadyParented {
			if alreadyParentedItem == childLabel {
				alreadyHasParent = true
			}
		}

		if !alreadyHasParent {
			node["children"] = append(node["children"], childLabel)
		}

		nodes[childLabel] = childNode
	}

	if noMetadata != len(remainingData) {
		fmt.Println("Something has gone wrong in parsing...")
	}

	for i := 0; i < noMetadata; i++ {
		node["metadata"] = append(node["metadata"], remainingData[i])
	}

	nodes[label] = node

	return nodes
}

func returnNodes(data []int, children int, label int) (map[int]map[string][]int, []int) {
	nodes := make(map[int]map[string][]int, 0)
	remainingData := data

	for c := 0; c < children; c++ {
		node := make(map[string][]int, 0)
		noChildren := remainingData[0]
		noMetadata := remainingData[1]

		if noChildren == 0 {
			for i := 0; i < noMetadata; i++ {
				metadata := remainingData[i + 2]
				node["metadata"] = append(node["metadata"], metadata)
			}

			nodes[label] = node

			remainingData = remainingData[2 + noMetadata:]
		} else {
			node["children"] = []int{}
			node["metadata"] = []int{}

			childNodes := make(map[int]map[string][]int, 0)

			childNodes, remainingData = returnNodes(remainingData[2:], noChildren, label + 1)

			alreadyParented := []int{}

			for _, childNode := range childNodes {
				alreadyParented = append(alreadyParented, childNode["children"]...)
			}

			for childLabel, childNode := range childNodes {
				alreadyHasParent := false

				for _, alreadyParentedItem := range alreadyParented {
					if alreadyParentedItem == childLabel {
						alreadyHasParent = true
					}
				}

				if !alreadyHasParent {
					node["children"] = append(node["children"], childLabel)
				}

				nodes[childLabel] = childNode
			}

			for m := 0; m < noMetadata; m++ {
				node["metadata"] = append(node["metadata"], remainingData[m])
			}

			remainingData = remainingData[noMetadata:]

			nodes[label] = node
		}

		label += 1
	}

	return nodes, remainingData
}