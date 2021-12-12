function getHorizontalEdgeLocations(input: number[][], i: number, j: number): [number, number][] {
    let edgeLocations: [number, number][] = [];

    if (i > 0) edgeLocations.push([i - 1, j]);
    if (i < (input.length - 1)) edgeLocations.push([i + 1, j]);
    if (j > 0) edgeLocations.push([i, j - 1]);
    if (j < (input[i].length - 1)) edgeLocations.push([i, j + 1]);

    return edgeLocations;
}

function getAllEdgeLocations(input: number[][], i: number, j: number): [number, number][] {
    let edgeLocations: [number, number][] = [];

    for (let ii = -1; ii <= 1; ii++) {
        for (let jj = -1; jj <= 1; jj++) {
            if (i + ii < input.length && i + ii >= 0 && j + jj < input[i].length && j + jj >= 0) {
                edgeLocations.push([i + ii, j + jj]);
            }
        }
    }

    return edgeLocations;
}

export { getHorizontalEdgeLocations, getAllEdgeLocations };
