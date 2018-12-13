package day9

import (
	"container/list"
	"fmt"
)

func Part1() {
	players := 476
	lastMarble := 71431
	marbles := list.New()
	currentMarble := marbles.PushBack(0)

	scores := make(map[int]int, 0)

	for player := 1; player <= players; player++ {
		scores[player] = 0
	}

	player := 1

	for marble := 1; marble <= lastMarble; marble++ {
		score := 0

		if marble%23 == 0 {
			marbleToRemove := currentMarble

			for i := 0; i < 7; i++ {
				marbleToRemove = marbleToRemove.Prev()

				if marbleToRemove == nil {
					marbleToRemove = marbles.Back()
				}
			}

			currentMarble = marbleToRemove.Next()
			score = marbleToRemove.Value.(int) + marble

			marbles.Remove(marbleToRemove)
		} else {
			insertAfter := currentMarble.Next()

			if insertAfter == nil {
				insertAfter = marbles.Front()
			}

			currentMarble = marbles.InsertAfter(marble, insertAfter)
		}

		scores[player] += score

		player = (player % players) + 1
	}

	maxScore := 0
	maxPlayer := 0

	for player, score := range scores {
		if score > maxScore {
			maxScore = score
			maxPlayer = player
		}
	}

	fmt.Print("Max Score of ")
	fmt.Print(maxScore)
	fmt.Print(" achieved by player ")
	fmt.Println(maxPlayer)
}

func Part2() {
	players := 476
	lastMarble := 7143100
	marbles := list.New()
	currentMarble := marbles.PushBack(0)

	scores := make(map[int]int, 0)

	for player := 1; player <= players; player++ {
		scores[player] = 0
	}

	player := 1

	for marble := 1; marble <= lastMarble; marble++ {
		score := 0

		if marble%23 == 0 {
			marbleToRemove := currentMarble

			for i := 0; i < 7; i++ {
				marbleToRemove = marbleToRemove.Prev()

				if marbleToRemove == nil {
					marbleToRemove = marbles.Back()
				}
			}

			currentMarble = marbleToRemove.Next()
			score = marbleToRemove.Value.(int) + marble

			marbles.Remove(marbleToRemove)
		} else {
			insertAfter := currentMarble.Next()

			if insertAfter == nil {
				insertAfter = marbles.Front()
			}

			currentMarble = marbles.InsertAfter(marble, insertAfter)
		}

		scores[player] += score

		player = (player % players) + 1
	}

	maxScore := 0
	maxPlayer := 0

	for player, score := range scores {
		if score > maxScore {
			maxScore = score
			maxPlayer = player
		}
	}

	fmt.Print("Max Score of ")
	fmt.Print(maxScore)
	fmt.Print(" achieved by player ")
	fmt.Println(maxPlayer)
}
