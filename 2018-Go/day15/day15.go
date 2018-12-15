package day15

import (
	"fmt"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
)

func Part1() {
	round, hitPointSum, _ := runBattle(3)

	fmt.Print("Game over, outcome: ")
	fmt.Print(round)
	fmt.Print(" rounds completed, ")
	fmt.Print(hitPointSum)
	fmt.Print(" hitpoints left, total: ")
	fmt.Println(round * hitPointSum)
}

func Part2() {
	elvesDead := true

	//Played with this manually to find the right kind of area before letting the program run so it didn't take forever
	elvesAttackPower := 19
	round := 0
	hitPointSum := 0

	for elvesDead {
		elvesAttackPower++

		fmt.Print("Running attack with power of ")
		fmt.Println(elvesAttackPower)

		round, hitPointSum, elvesDead = runBattle(elvesAttackPower)
	}

	fmt.Print("Elves need an attack power of ")
	fmt.Print(elvesAttackPower)
	fmt.Println(" to win with no deaths")

	fmt.Print("Game over, outcome: ")
	fmt.Print(round)
	fmt.Print(" rounds completed, ")
	fmt.Print(hitPointSum)
	fmt.Print(" hitpoints left, total: ")
	fmt.Println(round * hitPointSum)
}

func runBattle(elvesAttackPower int) (int, int, bool) {
	board, units := getInput(elvesAttackPower)

	round := 0
	gameOver := false
	elvesDead := false

	for !gameOver {
		//New Round
		round += 1
		allUnitsMovedThisRound := false

		//Each unit plays in a round in 'reading' order
		for !allUnitsMovedThisRound {
			//First, pick a unit to move...
			unitToMove := -1

			for y := 0; y < len(board); y++ {
				for x := 0; x < len(board[y]); x++ {
					for idx, unit := range units {
						if unit["x"] == x && unit["y"] == y && unit["played"] == 0 && unitToMove == -1 {
							unitToMove = idx
						}
					}
				}
			}

			units[unitToMove]["played"] = 1

			//First, is there an enemy in range?
			inRange := enemiesInRange(units, units[unitToMove])

			if len(inRange) == 0 {
				//Need to move first
				//Find Targets
				targets := []int{}

				for idx, unit := range units {
					if unit["team"] != units[unitToMove]["team"] {
						targets = append(targets, idx)
					}
				}

				//Find spaces In Range based on targets
				inRangeSpaces := make([]map[string]int, 0)
				obstaclesBoard := getObstaclesBoard(board, units)
				obstaclesBoard[units[unitToMove]["y"]][units[unitToMove]["x"]] = "."

				for _, target := range targets {
					if obstaclesBoard[units[target]["y"]-1][units[target]["x"]] == "." {
						inRangeSpace := make(map[string]int, 0)

						inRangeSpace["y"] = units[target]["y"] - 1
						inRangeSpace["x"] = units[target]["x"]

						inRangeSpaces = append(inRangeSpaces, inRangeSpace)
					}

					if obstaclesBoard[units[target]["y"]][units[target]["x"]-1] == "." {
						inRangeSpace := make(map[string]int, 0)

						inRangeSpace["y"] = units[target]["y"]
						inRangeSpace["x"] = units[target]["x"] - 1

						inRangeSpaces = append(inRangeSpaces, inRangeSpace)
					}

					if obstaclesBoard[units[target]["y"]][units[target]["x"]+1] == "." {
						inRangeSpace := make(map[string]int, 0)

						inRangeSpace["y"] = units[target]["y"]
						inRangeSpace["x"] = units[target]["x"] + 1

						inRangeSpaces = append(inRangeSpaces, inRangeSpace)
					}

					if obstaclesBoard[units[target]["y"]+1][units[target]["x"]] == "." {
						inRangeSpace := make(map[string]int, 0)

						inRangeSpace["y"] = units[target]["y"] + 1
						inRangeSpace["x"] = units[target]["x"]

						inRangeSpaces = append(inRangeSpaces, inRangeSpace)
					}
				}

				for idx, inRangeSpace := range inRangeSpaces {
					distanceBoard := findShortestPath(obstaclesBoard, units[unitToMove]["x"], units[unitToMove]["y"], inRangeSpace["x"], inRangeSpace["y"])

					if distanceBoard[units[unitToMove]["y"]][units[unitToMove]["x"]] != -1 {
						inRangeSpaces[idx]["reachable"] = 1
						inRangeSpaces[idx]["pathlen"] = distanceBoard[units[unitToMove]["y"]][units[unitToMove]["x"]]
					} else {
						inRangeSpaces[idx]["reachable"] = 0
					}
				}

				shortestPath := len(board) * len(board[0])

				for _, inRangeSpace := range inRangeSpaces {
					if inRangeSpace["reachable"] == 1 {
						if inRangeSpace["pathlen"] < shortestPath {
							shortestPath = inRangeSpace["pathlen"]
						}
					}
				}

				moveTowards := -1

				//Reading order...
				for y := 0; y < len(board); y++ {
					for x := 0; x < len(board[y]); x++ {
						for idx, inRangeSpace := range inRangeSpaces {
							if inRangeSpace["x"] == x && inRangeSpace["y"] == y && inRangeSpace["pathlen"] == shortestPath && moveTowards == -1 {
								moveTowards = idx
							}
						}
					}
				}

				if moveTowards != -1 {
					distanceBoard := findShortestPath(obstaclesBoard, units[unitToMove]["x"], units[unitToMove]["y"], inRangeSpaces[moveTowards]["x"], inRangeSpaces[moveTowards]["y"])

					if distanceBoard[units[unitToMove]["y"]-1][units[unitToMove]["x"]] == distanceBoard[units[unitToMove]["y"]][units[unitToMove]["x"]]-1 {
						units[unitToMove]["y"] = units[unitToMove]["y"] - 1
					} else if distanceBoard[units[unitToMove]["y"]][units[unitToMove]["x"]-1] == distanceBoard[units[unitToMove]["y"]][units[unitToMove]["x"]]-1 {
						units[unitToMove]["x"] = units[unitToMove]["x"] - 1
					} else if distanceBoard[units[unitToMove]["y"]][units[unitToMove]["x"]+1] == distanceBoard[units[unitToMove]["y"]][units[unitToMove]["x"]]-1 {
						units[unitToMove]["x"] = units[unitToMove]["x"] + 1
					} else if distanceBoard[units[unitToMove]["y"]+1][units[unitToMove]["x"]] == distanceBoard[units[unitToMove]["y"]][units[unitToMove]["x"]]-1 {
						units[unitToMove]["y"] = units[unitToMove]["y"] + 1
					}
				}
			}

			//Check again - is there an enemy in range?
			inRange = enemiesInRange(units, units[unitToMove])

			if len(inRange) > 0 {
				//Attack!

				//First, pick a target - lowest hitpoints wins
				lowestHP := 201

				for _, idx := range inRange {
					if units[idx]["hitpoints"] < lowestHP {
						lowestHP = units[idx]["hitpoints"]
					}
				}

				enemyToAttack := -1

				//In reading order
				for y := 0; y < len(board); y++ {
					for x := 0; x < len(board[y]); x++ {
						for _, idx := range inRange {
							//If we don't already have an enemy to attack and the hitpoints matches the lowest we found...
							if units[idx]["x"] == x && units[idx]["y"] == y && enemyToAttack == -1 && units[idx]["hitpoints"] == lowestHP {
								enemyToAttack = idx
							}
						}
					}
				}

				if enemyToAttack != -1 {
					//Attack the enemy...
					units[enemyToAttack]["hitpoints"] = units[enemyToAttack]["hitpoints"] - units[unitToMove]["attack"]

					if units[enemyToAttack]["hitpoints"] <= 0 {
						//He be dead... remove from the game

						if units[enemyToAttack]["team"] == 0 {
							elvesDead = true
						}

						units = append(units[:enemyToAttack], units[enemyToAttack+1:]...)
					}
				}
			}

			//Check if all units moved this round
			allUnitsMovedThisRound = true

			for _, unit := range units {
				if unit["played"] == 0 {
					allUnitsMovedThisRound = false
				}
			}

			//Check if game over
			teamsStillInGame := make(map[int]bool, 0)

			for _, unit := range units {
				teamsStillInGame[unit["team"]] = true
			}

			if len(teamsStillInGame) == 1 {
				gameOver = true

				if allUnitsMovedThisRound {
					//Count only FULL rounds
					round -= 1
				}
			}
		}

		//Reset 'played' bit on all remaining units
		for idx := range units {
			units[idx]["played"] = 0
		}
	}

	hitPointSum := 0

	for _, unit := range units {
		hitPointSum += unit["hitpoints"]
	}

	return round, hitPointSum, elvesDead
}

func getInput(elvesAttackPower int) (map[int]map[int]string, []map[string]int) {
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
					unit["attack"] = elvesAttackPower
				} else {
					unit["team"] = 1
					unit["attack"] = 3
				}

				unit["played"] = 0
				unit["hitpoints"] = 200

				units = append(units, unit)
			} else {
				board[y][x] = string(char)
			}
		}
	}

	return board, units
}

func enemiesInRange(units []map[string]int, unit map[string]int) []int {
	enemiesInRange := []int{}

	for idx, thisUnit := range units {
		if thisUnit["team"] != unit["team"] && thisUnit["y"] == unit["y"] && (thisUnit["x"]+1 == unit["x"] || thisUnit["x"]-1 == unit["x"]) {
			enemiesInRange = append(enemiesInRange, idx)
		}

		if thisUnit["team"] != unit["team"] && thisUnit["x"] == unit["x"] && (thisUnit["y"]+1 == unit["y"] || thisUnit["y"]-1 == unit["y"]) {
			enemiesInRange = append(enemiesInRange, idx)
		}
	}

	return enemiesInRange
}

func getObstaclesBoard(board map[int]map[int]string, units []map[string]int) map[int]map[int]string {
	obstaclesBoard := make(map[int]map[int]string, 0)

	for y := 0; y < len(board); y++ {
		obstaclesBoard[y] = make(map[int]string, 0)

		for x := 0; x < len(board[y]); x++ {
			unitHere := false

			for _, unit := range units {
				if unit["x"] == x && unit["y"] == y {
					unitHere = true
				}
			}

			if unitHere {
				obstaclesBoard[y][x] = "#"
			} else {
				obstaclesBoard[y][x] = board[y][x]
			}
		}
	}

	return obstaclesBoard
}

func findShortestPath(board map[int]map[int]string, srcx int, srcy int, dstx int, dsty int) map[int]map[int]int {
	distanceBoard := make(map[int]map[int]int, 0)

	for y := 0; y < len(board); y++ {
		distanceBoard[y] = make(map[int]int, 0)

		for x := 0; x < len(board[y]); x++ {
			if board[y][x] == "." {
				distanceBoard[y][x] = -1
			} else {
				distanceBoard[y][x] = 99999
			}
		}
	}

	distanceBoard[dsty][dstx] = 0
	level := 1
	spacePainted := true

	for spacePainted {
		spacePainted = false

		for y := 0; y < len(distanceBoard); y++ {
			for x := 0; x < len(distanceBoard[y]); x++ {

				if distanceBoard[y][x] == level-1 {
					if distanceBoard[y-1][x] == -1 {
						distanceBoard[y-1][x] = level
						spacePainted = true
					}

					if distanceBoard[y][x-1] == -1 {
						distanceBoard[y][x-1] = level
						spacePainted = true
					}

					if distanceBoard[y][x+1] == -1 {
						distanceBoard[y][x+1] = level
						spacePainted = true
					}

					if distanceBoard[y+1][x] == -1 {
						distanceBoard[y+1][x] = level
						spacePainted = true
					}
				}
			}
		}

		level += 1
	}

	return distanceBoard
}
