import * as fs from 'fs';
import {getHorizontalEdgeLocations} from "../Utils/grid";

function part1() {
    let input = getInput();
    let riskGrid = generateRiskGrid(input, input[0].length - 1, input.length - 1);

    console.log("Minimum risk route has risk level of " + riskGrid[0][0]);
}

function part2() {
    let wholeCave = generateWholeCaveFromInput(getInput());
    let riskGrid = generateRiskGrid(wholeCave, wholeCave[0].length - 1, wholeCave.length - 1);

    console.log("Minimum risk route through whole cave has risk level of " + riskGrid[0][0]);
}

function generateRiskGrid(grid: number[][], targetx: number, targety: number): number[][] {
    //Generate initial grid
    let riskGrid: number[][] = [];

    for (let y = 0; y < grid.length; y++) {
        riskGrid[y] = [];

        for (let x = 0; x < grid[y].length; x++) {
            riskGrid[y][x] = -1;
        }
    }

    riskGrid[targety][targetx] = 0;

    let stillGoing = true;

    while (stillGoing) {
        stillGoing = false;

        for (let y = 0; y < grid.length; y++) {
            for (let x = 0; x < grid[y].length; x++) {
                let riskScoresOfNeighbours: number[] = getHorizontalEdgeLocations(grid, x, y)
                    .filter(([xx, yy]) => riskGrid[yy][xx] !== -1)
                    .map(([xx, yy]) => riskGrid[yy][xx] + grid[yy][xx])

                if (riskScoresOfNeighbours.length > 0) {
                    let lowestRiskScore = Math.min(...riskScoresOfNeighbours);

                    if (riskGrid[y][x] == -1 || riskGrid[y][x] > lowestRiskScore) {
                        stillGoing = true;
                        riskGrid[y][x] = lowestRiskScore;
                    }
                }
            }
        }
    }

    return riskGrid;
}

function generateWholeCaveFromInput(grid: number[][]): number[][] {
    let cave: number[][] = [];

    for (let y = 0; y < grid.length * 5; y++) {
        cave[y] = [];

        for (let x = 0; x < grid[0].length * 5; x++) {
            cave[y][x] = ((grid[y % grid.length][x % grid[0].length] + Math.floor(y / grid.length) + Math.floor(x / grid[0].length) - 1) % 9) + 1;
        }
    }

    return cave;
}

function getInput(): number[][] {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split("\n")
        .map(l => l
            .split('')
            .map(d => parseInt(d)));
}

export {part1, part2};
