import * as fs from 'fs';

interface Point {
    x: number,
    y: number
}

type HeatMap = {
    [x: number]: {
        [y: number]: number
    }
}

function part1 () {
    console.log(getNumberOfCrossingPoints(false));
}

function part2 () {
    console.log(getNumberOfCrossingPoints(true));
}

function getNumberOfCrossingPoints(includeDiagonals = false): number {
    return Object.values(
        getInput()
            .map(l => returnAllPointsOnLine(l[0], l[1], includeDiagonals))
            .reduce((acc, pointsArray) => acc.concat(pointsArray), [])
            .reduce((heatMap: HeatMap, point: Point) => addPointToHeatmap(heatMap, point), {}))
        .map(yCoords => Object.values(yCoords).filter(y => y > 1).length)
        .reduce((total, current) => total + current, 0);
}

function addPointToHeatmap(heatMap: HeatMap, point: Point): HeatMap {
    if (!heatMap.hasOwnProperty(point.x)) {
        heatMap[point.x] = {};
    }

    if (!heatMap[point.x].hasOwnProperty(point.y)) {
        heatMap[point.x][point.y] = 0;
    }

    heatMap[point.x][point.y]++;

    return heatMap;
}

function returnAllPointsOnLine(end1: Point, end2: Point, considerDiagonals = false): Point[] {
    let points = [];

    if (end1.x === end2.x) {
        for (let y = Math.min(end1.y, end2.y); y <= Math.max(end1.y, end2.y); y++) {
            points.push({
                x: end1.x,
                y
            });
        }
    } else if (end1.y === end2.y) {
        for (let x = Math.min(end1.x, end2.x); x <= Math.max(end1.x, end2.x); x++) {
            points.push({
                x,
                y: end1.y
            });
        }
    } else if (considerDiagonals) {
        const xStep = Math.abs(end1.x - end2.x)/(end1.x - end2.x);
        const yStep = Math.abs(end1.y - end2.y)/(end1.y - end2.y);

        for (let i = 0; i <= Math.abs(end1.x - end2.x); i++) {
            points.push({
                x: end2.x + (i * xStep),
                y: end2.y + (i * yStep)
            });
        }
    }

    return points;
}

function getInput(): Point[][] {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .split("\n")
        .filter(l => l !== '')
        .map(l => l.split(' -> ').map(c => {
            const coords = c.split(',').map(s => parseInt(s))

            return {
                x: coords[0],
                y: coords[1]
            }
        }));
}

export { part1, part2 };
