package day15

import (
	"fmt"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
)

func Part1() {
	board, units := getInput()

	round := 0
	gameOver := false

	for !gameOver {
		//New Round
		round += 1
		allUnitsMovedThisRound := false

		//Each unit plays in a round in 'reading' order
		for allUnitsMovedThisRound {
			//First, pick a unit to move...

			unitToMove := 0

			for y, line := range board {
				for x := range line {
					for idx, unit := range units {
						if unit["x"] == x && unit["y"] == y && unit["played"] == 0 {
							unitToMove = idx
						}
					}
				}
			}

			//First, is there an enemy in range?
			inRange := enemiesInRange(units, units[unitToMove])

			if len(inRange) == 0 {
				//Need to move first












				









			}

			//Check again - is there an enemy in range?
			inRange = enemiesInRange(units, units[unitToMove])

			if len(inRange) > 0 {
				//Attack!

				//First, pick a target - lowest hitpoints wins
				lowestHP := 200

				for idx := range inRange {
					if units[idx]["hitpoints"] < lowestHP {
						lowestHP = units[idx]["hitpoints"]
					}
				}

				enemyToAttack := 0

				//In reading order
				for y, line := range board {
					for x := range line {
						for idx := range inRange {
							//If we don't already have an enemy to attack and the hitpoints matches the lowest we found...
							if units[idx]["x"] == x && units[idx]["y"] == y && enemyToAttack == 0 && units[idx]["hitpoints"] == lowestHP {
								enemyToAttack = idx
							}
						}
					}
				}

				//Attack the enemy...
				units[enemyToAttack]["hitpoints"] = units[enemyToAttack]["hitpoints"] - units[unitToMove]["attack"]

				if units[enemyToAttack]["hitpoints"] <= 0 {
					//He be dead... remove from the game

					units = append(units[:enemyToAttack], units[enemyToAttack + 1:]...)
				}
			}

			//Check if all units moved this round
			allUnitsMovedThisRound = true

			for _, unit := range units {
				if unit["played"] == 0 {
					allUnitsMovedThisRound = false
				}
			}
		}

		//Reset 'played' bit on all remaining units
		for idx := range units {
			units[idx]["played"] = 0
		}

		//Check if game over
		teamsStillInGame := make(map[int]bool, 0)

		for _, unit := range units {
			teamsStillInGame[unit["team"]] = true
		}

		if len(teamsStillInGame) == 1 {
			gameOver = true
		}
	}

	hitPointSum := 0

	for _, unit := range units {
		hitPointSum += unit["hitpoints"]
	}

	fmt.Print("Game over, outcome: ")
	fmt.Println(round * hitPointSum)
}

func Part2() {
}

func getInput() (map[int]map[int]string, []map[string]int) {
	input := util.GetInputAsArrayTrimmed("day15/input.txt")

	board := make(map[int]map[int]string, 0)
	units := make([]map[string]int, 0)

	for y, line := range input {
		board[y] = make(map[int]string, 0)

		for x, char := range line {
			if char == 'G' || char == 'E' {
				board[y][x] = "."

				unit := make(map[string]int, 0)

				unit["x"] = x
				unit["y"] = y

				if char == 'E' {
					unit["team"] = 0
				} else {
					unit["team"] = 1
				}

				unit["played"] = 0
				unit["hitpoints"] = 200
				unit["attack"] = 3

				units = append(units, unit)
			} else {
				board[y][x] = string(char)
			}
		}
	}

	return board, units
}

func enemiesInRange (units []map[string]int, unit map[string]int) []int {
	enemiesInRange := []int{}

	for idx, thisUnit := range units {
		if thisUnit["team"] != unit["team"] && thisUnit["y"] == unit["y"] && (thisUnit["x"] + 1 == unit["x"] || thisUnit["x"] - 1 == unit["x"]) {
			enemiesInRange = append(enemiesInRange, idx)
		}

		if thisUnit["team"] != unit["team"] && thisUnit["x"] == unit["x"] && (thisUnit["y"] + 1 == unit["y"] || thisUnit["y"] - 1 == unit["y"]) {
			enemiesInRange = append(enemiesInRange, idx)
		}
	}

	return enemiesInRange
}