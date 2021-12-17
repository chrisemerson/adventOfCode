import * as fs from 'fs';

function part1() {
    const [[minX, maxX], [minY, maxY]] = getInput();

    let highestY = 0;

    for (let x = 1; x <= maxX; x++) {
        for (let y = minY; y <= 1000; y++) {
            if (shotFallsWithinTargetArea(x, y, minX, maxX, minY, maxY) && y > highestY) {
                highestY = y;
            }
        }
    }

    const highestYPosition = highestY * (highestY + 1) / 2;

    console.log("Highest Y position: " + highestYPosition);
}

function part2() {
    const [[minX, maxX], [minY, maxY]] = getInput();

    let countOfInitialVelocities = 0;

    for (let x = 1; x <= maxX; x++) {
        for (let y = minY; y <= 1000; y++) {
            if (shotFallsWithinTargetArea(x, y, minX, maxX, minY, maxY)) {
                countOfInitialVelocities++;
            }
        }
    }

    console.log("Number of distinct starting velocities: " + countOfInitialVelocities);
}

function shotFallsWithinTargetArea(
    vX: number,
    vY: number,
    minX: number,
    maxX: number,
    minY: number,
    maxY: number
): boolean {
    let posX = 0, posY = 0;

    while (posX <= maxX && posY >= minY) {
        posX += vX;
        posY += vY;

        vX = Math.max(vX - 1, 0);
        vY -= 1;

        if (posX >= minX && posX <= maxX && posY >= minY && posY <= maxY) {
            return true;
        }
    }

    return false;
}

function getInput(): number[][] {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split('x=')[1]
        .split(', y=')
        .map(range => range
            .split('..')
            .map(d => parseInt(d)));
}

export {part1, part2};
