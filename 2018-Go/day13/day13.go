package day13

import (
	"fmt"
	"github.com/chrisemerson/AdventOfCode/2018-Go/util"
)

func Part1() {
	dirmap := make(map[rune]int, 4)

	dirmap['^'] = 0
	dirmap['>'] = 1
	dirmap['v'] = 2
	dirmap['<'] = 3

	track, carts := getInput(dirmap)
	collision := false
	colx := 0
	coly := 0

	for !collision {
		for y := 0; y < len(track); y++ {
			for x := 0; x < len(track[y]); x++ {
				cartFound := false

				for idx, cart := range carts {
					if cart["y"] == y && cart["x"] == x && cart["m"] == 0 {
						cartFound = true

						switch cart["d"] {
						case 0:
							carts[idx]["y"] -= 1

						case 1:
							carts[idx]["x"] += 1

						case 2:
							carts[idx]["y"] += 1

						case 3:
							carts[idx]["x"] -= 1
						}

						cart["m"] = 1

						carts[idx] = updateCardDirection(cart, track[carts[idx]["y"]][carts[idx]["x"]])
					}
				}

				if cartFound {
					collision, colx, coly = checkForCollisions(carts)

					if collision {
						fmt.Print("Collision found at ")
						fmt.Print(colx)
						fmt.Print(",")
						fmt.Println(coly)

						return
					}
				}
			}
		}

		for idx := range carts {
			carts[idx]["m"] = 0
		}
	}
}

func Part2() {
	dirmap := make(map[rune]int, 4)

	dirmap['^'] = 0
	dirmap['>'] = 1
	dirmap['v'] = 2
	dirmap['<'] = 3

	track, carts := getInput(dirmap)
	collision := false
	colx := 0
	coly := 0

	for len(carts) > 1 {
		for y := 0; y < len(track); y++ {
			for x := 0; x < len(track[y]); x++ {
				cartFound := false

				for idx, cart := range carts {
					if cart["y"] == y && cart["x"] == x && cart["m"] == 0 {
						cartFound = true

						switch cart["d"] {
						case 0:
							carts[idx]["y"] -= 1

						case 1:
							carts[idx]["x"] += 1

						case 2:
							carts[idx]["y"] += 1

						case 3:
							carts[idx]["x"] -= 1
						}

						cart["m"] = 1

						carts[idx] = updateCardDirection(cart, track[carts[idx]["y"]][carts[idx]["x"]])
					}
				}

				if cartFound {
					collision, colx, coly = checkForCollisions(carts)

					if collision {
						fmt.Print("Collision found at ")
						fmt.Print(colx)
						fmt.Print(",")
						fmt.Println(coly)

						newCarts := make([]map[string]int, 0)

						for _, cart := range carts {
							if cart["x"] != colx || cart["y"] != coly {
								newCarts = append(newCarts, cart)
							}
						}

						carts = newCarts

						fmt.Print(len(carts))
						fmt.Println(" carts left")
					}
				}
			}
		}

		for idx := range carts {
			carts[idx]["m"] = 0
		}

		if len(carts) == 1 {
			fmt.Print("Last cart ends up at ")
			fmt.Print(carts[0]["x"])
			fmt.Print(",")
			fmt.Println(carts[0]["y"])

			return
		}
	}
}

func getInput(dirmap map[rune]int) (map[int]map[int]string, []map[string]int) {
	track := make(map[int]map[int]string, 0)
	carts := make([]map[string]int, 0)

	input := util.GetInputAsArray("day13/input.txt")

	for y, line := range input {
		track[y] = make(map[int]string, 0)

		for x, char := range line {
			if char == '<' || char == '>' || char == '^' || char == 'v' {
				cart := make(map[string]int, 0)

				cart["x"] = x
				cart["y"] = y
				cart["d"] = dirmap[char]
				cart["l"] = 2
				cart["m"] = 0

				carts = append(carts, cart)

				if char == '<' || char == '>' {
					track[y][x] = "-"
				} else {
					track[y][x] = "|"
				}
			} else {
				track[y][x] = string(char)
			}
		}
	}

	return track, carts
}

func updateCardDirection(cart map[string]int, trackPiece string) map[string]int {
	if trackPiece == "/" {
		if cart["d"]%2 == 0 {
			cart["d"] = (cart["d"] + 1) % 4
		} else {
			cart["d"] = (cart["d"] + 3) % 4
		}
	}

	if trackPiece == "\\" {
		if cart["d"]%2 == 0 {
			cart["d"] = (cart["d"] + 3) % 4
		} else {
			cart["d"] = (cart["d"] + 1) % 4
		}
	}

	if trackPiece == "+" {
		cart["l"] = (cart["l"] + 1) % 3

		switch cart["l"] {
		case 0:
			cart["d"] = (cart["d"] + 3) % 4
		case 2:
			cart["d"] = (cart["d"] + 1) % 4
		}
	}

	return cart
}

func checkForCollisions(carts []map[string]int) (bool, int, int) {
	for idxa, carta := range carts {
		for idxb, cartb := range carts {
			if idxa != idxb && carta["x"] == cartb["x"] && carta["y"] == cartb["y"] {
				return true, carta["x"], carta["y"]
			}
		}
	}

	return false, 0, 0
}

func displayTrack(track map[int]map[int]string, carts []map[string]int, dirmap map[rune]int) {
	for y := 0; y < len(track); y++ {
		for x := 0; x < len(track[y]); x++ {
			cartFound := false

			for _, cart := range carts {
				if cart["x"] == x && cart["y"] == y {
					cartFound = true

					for symbol, no := range dirmap {
						if cart["d"] == no {
							fmt.Print(string(symbol))
						}
					}
				}
			}

			if !cartFound {
				fmt.Print(track[y][x])
			}
		}

		fmt.Println("")
	}

	fmt.Println("")
}
