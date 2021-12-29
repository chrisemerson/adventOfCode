import fs from "fs";

function part1() {
    let grid: string[][]|false = getInput();
    let count = 0;

    while (grid !== false) {
        grid = moveSeaCucumbers(grid);
        count++;
    }

    console.log("Sea cucumbers stop moving after " + count + " steps");
}

function moveSeaCucumbers(grid: string[][]): string[][]|false {
    let cucumbersToMove: [number, number][] = [];
    let cucumbersMoved = 0;

    for (let i = 0; i < grid.length; i++) {
        for (let j = 0; j < grid[i].length; j++) {
            if (grid[i][j] === ">" && grid[i][(j + 1) % grid[i].length] === ".") {
                cucumbersToMove.push([i, j]);
            }
        }
    }

    for (let [i, j] of cucumbersToMove) {
        grid[i][j] = ".";
        grid[i][(j + 1) % grid[i].length] = ">";
    }

    cucumbersMoved += cucumbersToMove.length;
    cucumbersToMove = [];

    for (let i = 0; i < grid.length; i++) {
        for (let j = 0; j < grid[i].length; j++) {
            if (grid[i][j] === "v" && grid[(i + 1) % grid.length][j] === ".") {
                cucumbersToMove.push([i, j]);
            }
        }
    }

    for (let [i, j] of cucumbersToMove) {
        grid[i][j] = ".";
        grid[(i + 1) % grid.length][j] = "v";
    }

    cucumbersMoved += cucumbersToMove.length;

    if (cucumbersMoved === 0) {
        return false;
    }

    return grid;
}

function getInput(): string[][] {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split('\n')
        .map(line => line.split(''));
}

export {part1};
