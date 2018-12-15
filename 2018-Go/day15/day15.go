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
		for !allUnitsMovedThisRound {
			//First, pick a unit to move...

			unitToMove := 0

			for y, line := range board {
				for x := range line {
					for idx, unit := range units {
						if unit["x"] == x && unit["y"] == y && unit["played"] == 0 && unitToMove == 0 {
							unitToMove = idx
						}
					}
				}
			}

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

				for _, target := range targets {
					if obstaclesBoard[units[target]["y"] - 1][units[target]["x"]] == "." {
						inRangeSpace := make(map[string]int, 0)

						inRangeSpace["y"] = units[target]["y"] - 1
						inRangeSpace["x"] = units[target]["x"]

						inRangeSpaces = append(inRangeSpaces, inRangeSpace)
					}

					if obstaclesBoard[units[target]["y"]][units[target]["x"] - 1] == "." {
						inRangeSpace := make(map[string]int, 0)

						inRangeSpace["y"] = units[target]["y"]
						inRangeSpace["x"] = units[target]["x"] - 1

						inRangeSpaces = append(inRangeSpaces, inRangeSpace)
					}

					if obstaclesBoard[units[target]["y"]][units[target]["x"] + 1] == "." {
						inRangeSpace := make(map[string]int, 0)

						inRangeSpace["y"] = units[target]["y"]
						inRangeSpace["x"] = units[target]["x"] + 1

						inRangeSpaces = append(inRangeSpaces, inRangeSpace)
					}

					if obstaclesBoard[units[target]["y"] + 1][units[target]["x"]] == "." {
						inRangeSpace := make(map[string]int, 0)

						inRangeSpace["y"] = units[target]["y"] + 1
						inRangeSpace["x"] = units[target]["x"]

						inRangeSpaces = append(inRangeSpaces, inRangeSpace)
					}
				}

				for idx, inRangeSpace := range inRangeSpaces {
					path, reachable := findShortestPath(obstaclesBoard, units[unitToMove]["x"], units[unitToMove]["y"], inRangeSpace["x"], inRangeSpace["y"])

					if reachable {
						inRangeSpaces[idx]["reachable"] = 1
						inRangeSpaces[idx]["pathlen"] = len(path)
						inRangeSpaces[idx]["movex"] = path[0]["x"]
						inRangeSpaces[idx]["movey"] = path[0]["y"]
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
				for y, row := range board {
					for x := range row {
						for idx, inRangeSpace := range inRangeSpaces {
							if inRangeSpace["x"] == x && inRangeSpace["y"] == y && inRangeSpace["pathlen"] == shortestPath && moveTowards == -1 {
								moveTowards = idx
							}
						}
					}
				}

				if moveTowards != -1 {
					units[unitToMove]["x"] = inRangeSpaces[moveTowards]["movex"]
					units[unitToMove]["y"] = inRangeSpaces[moveTowards]["movey"]
				}
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

func getObstaclesBoard(board map[int]map[int]string, units []map[string]int) map[int]map[int]string {
	obstaclesBoard := make(map[int]map[int]string, 0)

	for y, row := range board {
		obstaclesBoard[y] = make(map[int]string, 0)

		for x, cell := range row {
			unitHere := false

			for _, unit := range units {
				if unit["x"] == x && unit["y"] == y {
					unitHere = true
				}
			}

			if unitHere {
				obstaclesBoard[y][x] = "#"
			} else {
				obstaclesBoard[y][x] = cell
			}
		}
	}

	return obstaclesBoard
}

func findShortestPath(board map[int]map[int]string, srcx int, srcy int, dstx int, dsty int) ([]map[string]int, bool) {
	path := make([]map[string]int, 0)
	maxPathLen := len(board) * len(board[0])

	if srcx == dstx && srcy == dsty {
		return path, true
	}

	//Find shortest path in each direction from current 'end' spot
	possiblePaths := make([][]map[string]int, 0)
	newCoord := make(map[string]int, 0)

	if board[srcx][srcy - 1] == "." {
		upPath, upBlocked := findShortestPath(board, srcx, srcy - 1, dstx, dsty)

		newCoord["x"] = srcx
		newCoord["y"] = srcy - 1

		path = append(path, newCoord)

		if !upBlocked && !checkDoubleBack(newCoord, upPath) && len(upPath) < maxPathLen {
			possiblePaths = append(possiblePaths, append(path, upPath...))
		}
	}

	if board[srcx - 1][srcy] == "." {
		leftPath, leftBlocked := findShortestPath(board, srcx - 1, srcy, dstx, dsty)

		newCoord["x"] = srcx - 1
		newCoord["y"] = srcy

		path = append(path, newCoord)

		if !leftBlocked && !checkDoubleBack(newCoord, leftPath) && len(leftPath) < maxPathLen {
			possiblePaths = append(possiblePaths, append(path, leftPath...))
		}
	}

	if board[srcx + 1][srcy] == "." {
		rightPath, rightBlocked := findShortestPath(board, srcx + 1, srcy, dstx, dsty)

		newCoord["x"] = srcx + 1
		newCoord["y"] = srcy

		path = append(path, newCoord)

		if !rightBlocked && !checkDoubleBack(newCoord, rightPath) && len(rightPath) < maxPathLen {
			possiblePaths = append(possiblePaths, append(path, rightPath...))
		}
	}

	if board[srcx][srcy + 1] == "." {
		downPath, downBlocked := findShortestPath(board, srcx, srcy + 1, dstx, dsty)

		newCoord["x"] = srcx
		newCoord["y"] = srcy + 1

		path = append(path, newCoord)

		if !downBlocked && !checkDoubleBack(newCoord, downPath) && len(downPath) < maxPathLen {
			possiblePaths = append(possiblePaths, append(path, downPath...))
		}
	}

	shortestPath := maxPathLen

	for _, possiblePath := range possiblePaths {
		if len(possiblePath) < shortestPath {
			shortestPath = len(possiblePath)
		}
	}

	for _, possiblePath := range possiblePaths {
		if len(possiblePath) == shortestPath {
			return possiblePath, true
		}
	}

	return path, false
}

func checkDoubleBack(coord map[string]int, path []map[string]int) bool {
	for _, pathCoord := range path {
		if pathCoord["x"] == coord["x"] && pathCoord["y"] == coord["y"] {
			return true
		}
	}

	return false
}