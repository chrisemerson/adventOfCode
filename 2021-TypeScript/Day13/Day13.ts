import * as fs from 'fs';

function part1() {
    let [grid, foldInstructions] = getInput();

    displayGrid(grid);

    let gridAfterOneFold = foldGrid(grid, foldInstructions[0][0], foldInstructions[0][1]);

    const dotsVisible = gridAfterOneFold
        .map(row => row.filter(cell => cell).length)
        .reduce((acc, x) => acc + x, 0);

    console.log("After 1 fold, there are " + dotsVisible + " dots visible");
}

function part2() {
    let [grid, foldInstructions] = getInput();

    grid = foldInstructions.reduce((grid, fi) => foldGrid(grid, fi[0], fi[1]), grid);

    console.log("Grid after all folds completed:");
    console.log();

    displayGrid(grid);
}

function displayGrid(grid: boolean[][]): void
{
    for (let y = 0; y < grid.length; y++) {
        let rowString = '';

        for (let x = 0; x < grid[y].length; x++) {
            if (grid[y][x]) {
                rowString += '█';
            } else {
                rowString += ' ';
            }
        }

        console.log(rowString);
    }

    console.log();
}

function foldGrid(grid: boolean[][], foldDirection: string, foldLine: number): boolean[][] {
    let newGrid: boolean[][] = [];

    let newGridY = grid.length, newGridX = grid[0].length;

    if (foldDirection === 'y') {
        newGridY = foldLine;
    } else if (foldDirection === 'x') {
        newGridX = foldLine;
    }

    for (let y = 0; y < newGridY; y++) {
        newGrid[y] = [];

        for (let x = 0; x < newGridX; x++) {
            let mirrorY = 0, mirrorX = 0;

            if (foldDirection === 'y') {
                mirrorX = x;
                mirrorY = foldLine + Math.abs(y - foldLine);
            } else if (foldDirection === 'x') {
                mirrorX = foldLine + Math.abs(x - foldLine);
                mirrorY = y;
            }

            newGrid[y][x] = (grid[y][x] || grid[mirrorY][mirrorX]);
        }
    }

    return newGrid;
}

function getInput(): [boolean[][], [string, number][]] {
    let inputParts = fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split("\n\n");

    let dots = inputParts[0]
        .split("\n")
        .map(l => l
            .split(',')
            .map(d => parseInt(d)));

    let [maxx, maxy] = dots
        .reduce(([maxx, maxy], coord) => [Math.max(maxx, coord[0]), Math.max(maxy, coord[1])], [0, 0]);

    return [
        dots.reduce((grid: boolean[][], [x, y]: number[]) => {
            grid[y][x] = true;
            return grid;
        }, Array.from({length: maxy + 1}, _ => Array.from({length: maxx + 1}, _ => false))),
        inputParts[1]
            .split("\n")
            .map(fi => fi
                .replace("fold along ", '')
                .split('='))
            .map(fi => [fi[0], parseInt(fi[1])])
    ];
}

export {part1, part2};
