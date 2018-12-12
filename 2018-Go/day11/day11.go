package day11

import (
	"fmt"
	"math"
)

func Part1() {
	width := 300
	height := 300

	fuelGrid := getFuelGrid(width, height, 6878)

	maxPowerX, maxPowerY, _ := findMaxPower(fuelGrid, 3)

	fmt.Print("Max power is at Coordinates ")
	fmt.Print(maxPowerX)
	fmt.Print(",")
	fmt.Println(maxPowerY)
}

func Part2() {
	width := 300
	height := 300

	fuelGrid := getFuelGrid(width, height, 6878)

	maxPower := 0
	maxPowerX := 0
	maxPowerY := 0
	maxPowerWindowSize := 0

	//Observed that over this value, all power grids turn out <= 0
	for i := 1; i <= 25; i++ {
		powerX, powerY, power := findMaxPower(fuelGrid, i)

		if power > maxPower {
			maxPower = power
			maxPowerX = powerX
			maxPowerY = powerY
			maxPowerWindowSize = i
		}

		fmt.Print("Max power for window size ")
		fmt.Print(i)
		fmt.Print(" is ")
		fmt.Println(power)
	}

	fmt.Print("Max power is at Coordinates ")
	fmt.Print(maxPowerX)
	fmt.Print(",")
	fmt.Print(maxPowerY)
	fmt.Print(" with a window size of ")
	fmt.Print(maxPowerWindowSize)
}

func getFuelGrid(width int, height int, serial int) map[int]map[int]int {
	grid := make(map[int]map[int]int, 0)

	//Populate fuel grid
	for x := 1; x <= width; x++ {
		grid[x] = make(map[int]int, 0)

		for y := 1; y <= height; y++ {
			grid[x][y] = getFuelLevel(x, y, serial)
		}
	}

	return grid
}

func getFuelLevel(x int, y int, serial int) int {
	//Find the fuel cell's rack ID, which is its X coordinate plus 10.
	rackID := x + 10

	//Begin with a power level of the rack ID times the Y coordinate.
	power := rackID * y

	//Increase the power level by the value of the grid serial number (your puzzle input).
	power += serial

	//Set the power level to itself multiplied by the rack ID.
	power *= rackID

	//Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
	power = int(math.Floor(float64(power)/100)) % 10

	//Subtract 5 from the power level.
	power -= 5

	return power
}

func findMaxPower(grid map[int]map[int]int, windowSize int) (int, int, int) {
	maxPower := 0
	maxPowerX := 0
	maxPowerY := 0

	for x := 1; x <= (len(grid) - windowSize + 1); x++ {
		for y := 1; y <= (len(grid[x]) - windowSize + 1); y++ {
			power := 0

			for i := 0; i < windowSize; i++ {
				for j := 0; j < windowSize; j++ {
					power += grid[x+i][y+j]
				}
			}

			if power > maxPower {
				maxPower = power
				maxPowerX = x
				maxPowerY = y
			}
		}
	}

	return maxPowerX, maxPowerY, maxPower
}
