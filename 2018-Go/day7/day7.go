package day7

import (
	"fmt"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
	"regexp"
)

func Part1() {
	steps := getDependencyMap()

	fmt.Print("Order steps should be taken in: ")

	for len(steps) > 0 {
		stepTaken, newSteps := takeNextStep(steps)

		fmt.Print(stepTaken)
		steps = newSteps
	}

	fmt.Println("")
}

func Part2() {
	steps := getDependencyMap()
	time := 0
	workersAvailable := 5
	timeLeftPerStep := make(map[string]int)

	for true {
		//Check if any steps have finished (time left == 0), reallocate workers & remove deps
		for step, timeLeft := range timeLeftPerStep {
			if timeLeft == 0 {
				workersAvailable += 1
				delete(timeLeftPerStep, step)
				delete(steps, step)

				for stepLetter, deps := range steps {
					for idx, dep := range deps {
						if dep == step {
							steps[stepLetter] = append(deps[:idx], deps[idx+1:]...)
						}
					}
				}
			}
		}

		//Allocate available Workers to next steps
		for workersAvailable > 0 {
			taskAllocated := false

			for i := 1; i <= 26; i++ {
				stringToTest := string(rune('A' - 1 + i))

				if _, ok := steps[stringToTest]; ok {
					if len(steps[stringToTest]) == 0 {
						if _, ok := timeLeftPerStep[stringToTest]; !ok {
							timeLeftPerStep[stringToTest] = 60 + i
							workersAvailable -= 1
							taskAllocated = true
							break
						}
					}
				}
			}

			if !taskAllocated {
				//No steps available at the moment, break
				break
			}
		}

		//Are we done?
		if len(steps) == 0 {
			fmt.Print("Time taken to complete: ")
			fmt.Print(time)

			return
		}

		//Reduce time needed on all steps by 1
		for step := range timeLeftPerStep {
			timeLeftPerStep[step] -= 1
		}

		//Increase time
		time += 1
	}
}

func getDependencyMap() map[string][]string {
	deps := util.GetInputAsArrayTrimmed("day7/input.txt")
	steps := make(map[string][]string, 0)

	depRegex := regexp.MustCompile(`Step ([A-Z]) must be finished before step ([A-Z]) can begin.`)

	for _, dep := range deps {
		depSteps := depRegex.FindStringSubmatch(dep)

		if _, ok := steps[depSteps[2]]; !ok {
			steps[depSteps[2]] = make([]string, 0)
		}

		if _, ok := steps[depSteps[1]]; !ok {
			steps[depSteps[1]] = make([]string, 0)
		}

		steps[depSteps[2]] = append(steps[depSteps[2]], depSteps[1])
	}

	return steps
}

func takeNextStep(steps map[string][]string) (string, map[string][]string) {
	for i := 1; i <= 26; i++ {
		stringToTest := string(rune('A' - 1 + i))

		if _, ok := steps[stringToTest]; ok {
			if len(steps[stringToTest]) == 0 {
				delete(steps, stringToTest)

				for step, deps := range steps {
					for idx, dep := range deps {
						if dep == stringToTest {
							steps[step] = append(deps[:idx], deps[idx+1:]...)
						}
					}
				}

				return stringToTest, steps
			}
		}
	}

	panic("No steps can be taken!")
}
