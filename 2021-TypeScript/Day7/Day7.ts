import * as fs from 'fs';

function part1 () {
    findMinimumFuel(x => Math.abs(x));
}

function part2 () {
    findMinimumFuel(x => getNthTriangularNumber(Math.abs(x)));
}

function findMinimumFuel(fuelCallback: (_: number) => number): void {
    let input = getInput();

    let fuelPerPosition: {[_: number]: number} = {};

    for (let pos = 0; pos <= Math.max(...input); pos++) {
        fuelPerPosition[pos] = input.map(x => fuelCallback(pos - x)).reduce((acc, x) => acc + x, 0);
    }

    const minFuel = Math.min(...Object.values(fuelPerPosition));

    console.log("Minimum fuel requirement is: " + minFuel);
}

function getNthTriangularNumber(n: number): number {
    return n * (n + 1) / 2;
}

function getInput(): number[] {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split(',')
        .map(c => parseInt(c))
}

export { part1, part2 };
