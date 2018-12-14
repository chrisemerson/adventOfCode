package day14

import (
	"fmt"
	"math"
)

func Part1() {
	recipies := []int{3, 7}
	numrecipies := 825401

	elf1pos := 0
	elf2pos := 1

	for len(recipies) < numrecipies+10 {
		recipe1 := recipies[elf1pos]
		recipe2 := recipies[elf2pos]

		newrecipe := recipe1 + recipe2

		if newrecipe >= 10 {
			recipies = append(recipies, int(math.Floor(float64(newrecipe)/10.0)), newrecipe%10)
		} else {
			recipies = append(recipies, newrecipe)
		}

		elf1pos = (elf1pos + recipies[elf1pos] + 1) % len(recipies)
		elf2pos = (elf2pos + recipies[elf2pos] + 1) % len(recipies)
	}

	fmt.Println(recipies[numrecipies : numrecipies+10])
}

func Part2() {
	recipies := []int{3, 7}

	elf1pos := 0
	elf2pos := 1

	for true {
		recipe1 := recipies[elf1pos]
		recipe2 := recipies[elf2pos]

		newrecipe := recipe1 + recipe2

		if newrecipe >= 10 {
			recipies = append(recipies, int(math.Floor(float64(newrecipe)/10.0)), newrecipe%10)

			if len(recipies) >= 7 {
				if recipies[len(recipies)-7] == 8 && recipies[len(recipies)-6] == 2 && recipies[len(recipies)-5] == 5 && recipies[len(recipies)-4] == 4 && recipies[len(recipies)-3] == 0 && recipies[len(recipies)-2] == 1 {
					fmt.Print("Recipes to left of 825401: ")
					fmt.Println(len(recipies) - 7)

					return
				}

				if recipies[len(recipies)-6] == 8 && recipies[len(recipies)-5] == 2 && recipies[len(recipies)-4] == 5 && recipies[len(recipies)-3] == 4 && recipies[len(recipies)-2] == 0 && recipies[len(recipies)-1] == 1 {
					fmt.Print("Recipes to left of 825401: ")
					fmt.Println(len(recipies) - 6)

					return
				}
			}
		} else {
			recipies = append(recipies, newrecipe)

			if len(recipies) >= 6 && recipies[len(recipies)-6] == 8 && recipies[len(recipies)-5] == 2 && recipies[len(recipies)-4] == 5 && recipies[len(recipies)-3] == 4 && recipies[len(recipies)-2] == 0 && recipies[len(recipies)-1] == 1 {
				fmt.Print("Recipes to left of 825401: ")
				fmt.Println(len(recipies) - 6)

				return
			}
		}

		elf1pos = (elf1pos + recipies[elf1pos] + 1) % len(recipies)
		elf2pos = (elf2pos + recipies[elf2pos] + 1) % len(recipies)
	}
}
