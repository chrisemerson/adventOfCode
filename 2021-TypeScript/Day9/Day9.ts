import * as fs from 'fs';

function part1() {
    const input = getInput();

    let lowPoints = [];

    for (let i = 0; i < input.length; i++) {
        for (let j = 0; j < input[i].length; j++) {
            if (input[i][j] < Math.min(...getEdgeLocations(input, i, j).map(([i, j]) => input[i][j]))) {
                lowPoints.push(input[i][j]);
            }
        }
    }

    console.log("Risk Score: " + (lowPoints.reduce((acc, x) => acc + x, 0) + lowPoints.length));
}

function part2() {
    const input = getInput();

    let basinSizes = [];

    for (let i = 0; i < input.length; i++) {
        for (let j = 0; j < input[i].length; j++) {
            if (input[i][j] < Math.min(...getEdgeLocations(input, i, j).map(([i, j]) => input[i][j]))) {
                //Have to do this nonsense because tuple equality is stupid
                let basin = [
                    ...new Set(
                        findBasinFromStartingPoint(input, i, j)
                            .map(([i, j]) => i + '-' + j)
                    )]
                    .map(ij => ij.split('-').map(d => parseInt(d)));

                basinSizes.push(basin.length);
            }
        }
    }

    console.log(
        "Basin size multiplication result: "
        + basinSizes.sort((a, b) => b - a).slice(0, 3).reduce((acc, x) => acc * x, 1)
    );
}

function findBasinFromStartingPoint(input: number[][], i: number, j: number): [number, number][] {
    return [[i, j]].concat(
        ...getEdgeLocations(input, i, j)
            .filter(([ie, je]) => input[ie][je] > input[i][j] && input[ie][je] < 9)
            .map(([ie, je]) => findBasinFromStartingPoint(input, ie, je))
    ) as [number, number][];
}

function getEdgeLocations(input: number[][], i: number, j: number): number[][] {
    let edgeLocations: [number, number][] = [];

    if (i > 0) edgeLocations.push([i - 1, j]);
    if (i < (input.length - 1)) edgeLocations.push([i + 1, j]);
    if (j > 0) edgeLocations.push([i, j - 1]);
    if (j < (input[i].length - 1)) edgeLocations.push([i, j + 1]);

    return edgeLocations;
}

function getInput(): number[][] {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split("\n")
        .map(l => l.split(''))
        .map(l => l.map(d => parseInt(d)));
}

export {part1, part2};
