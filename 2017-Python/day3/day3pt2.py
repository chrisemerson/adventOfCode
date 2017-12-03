import sys

number = int(sys.argv[1])
grid = [[0 for i in range(0,1000)] for j in range(0, 1000)]

def updatePos(currentpos, dir):
    return {
        "R": [currentpos[0] + 1, currentpos[1]],
        "U": [currentpos[0], currentpos[1] - 1],
        "L": [currentpos[0] - 1, currentpos[1]],
        "D": [currentpos[0], currentpos[1] + 1]
    }[dir]

def sumNeighbours(currentpos):
    global grid
    return (
        grid[currentpos[0] - 1][currentpos[1] - 1] +
        grid[currentpos[0]][currentpos[1] - 1] +
        grid[currentpos[0] + 1][currentpos[1] - 1] +
        grid[currentpos[0] - 1][currentpos[1]] +
        grid[currentpos[0] + 1][currentpos[1]] +
        grid[currentpos[0] - 1][currentpos[1] + 1] +
        grid[currentpos[0]][currentpos[1] + 1] +
        grid[currentpos[0] + 1][currentpos[1] + 1]
    )

def nextDir(dir):
    return {
        "R": "U",
        "U": "L",
        "L": "D",
        "D": "R"
    }[dir]

currentpos = [500,500]
currentdir = 'R'

grid[currentpos[0]][currentpos[1]] = 1

while 1:
    # Move in currentdir direction
    currentpos = updatePos(currentpos, currentdir)

    # Populate space with sum of neighbours
    grid[currentpos[0]][currentpos[1]] = sumNeighbours(currentpos)

    # Update direction if needed
    newdir = nextDir(currentdir)
    newpos = updatePos(currentpos, newdir)
    if grid[newpos[0]][newpos[1]] == 0:
        currentdir = newdir

    # Check to see if we have just exceeded the input
    if sumNeighbours(currentpos) > number:
        print("Input exceeded with number: " + str(sumNeighbours(currentpos)))
        break
