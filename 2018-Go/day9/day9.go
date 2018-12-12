package day9

import (
	"fmt"
)

func Part1() {
	players := 476
	lastMarble := 71431
	marbles := []int{0}
	currentMarblePosition := 0

	scores := make(map[int]int, 0)

	for player := 1; player <= players; player++ {
		scores[player] = 0
	}

	player := 1

	for marble := 1; marble <= lastMarble; marble++ {
		score := 0
		marbles, currentMarblePosition, score = addMarble(marbles, currentMarblePosition, marble)
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
	marbles := []int{0}
	currentMarblePosition := 0

	scores := make(map[int]int, 0)

	for player := 1; player <= players; player++ {
		scores[player] = 0
	}

	player := 1

	for marble := 1; marble <= lastMarble; marble++ {
		score := 0
		marbles, currentMarblePosition, score = addMarble(marbles, currentMarblePosition, marble)
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

func addMarble(marbles []int, currentMarblePosition int, marbleToPlace int) ([]int, int, int) {
	if marbleToPlace%23 == 0 {
		marblePositionToRemove := (currentMarblePosition - 7 + len(marbles)) % len(marbles)
		score := marbleToPlace + marbles[marblePositionToRemove]

		return append(marbles[:marblePositionToRemove], marbles[marblePositionToRemove+1:]...), marblePositionToRemove % (len(marbles) - 1), score
	} else {
		newMarblePosition := (currentMarblePosition + 2) % len(marbles)
		marblesPost := make([]int, len(marbles[newMarblePosition:]))
		copy(marblesPost, marbles[newMarblePosition:])

		return append(append(marbles[:newMarblePosition], marbleToPlace), marblesPost...), newMarblePosition, 0
	}
}
