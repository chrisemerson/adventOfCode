import * as fs from 'fs';

type coords = { [_: string]: [number, number, number][] }

function part1() {
    const scannerMap = getFullBeaconMap();

    console.log("Total number of beacons: " + scannerMap.length);
}

function part2() {
    const scannerMap = getAllScannerOffsets();

    let maxManhattanDistance = 0;

    for (let i in scannerMap) {
        for (let j in scannerMap) {
            let thisManhattanDifference =
                Math.abs(scannerMap[i][0] - scannerMap[j][0])
                + Math.abs(scannerMap[i][1] - scannerMap[j][1])
                + Math.abs(scannerMap[i][2] - scannerMap[j][2]);

            if (thisManhattanDifference > maxManhattanDistance) {
                maxManhattanDistance = thisManhattanDifference;
            }
        }
    }

    console.log("Maximum distance between 2 beacons: " + maxManhattanDistance);
}

function getFullBeaconMap(): [number, number, number][] {
    return getScannerAndBeaconInfo()[0];
}

function getAllScannerOffsets(): {[_:string]: [number, number, number]} {
    return getScannerAndBeaconInfo()[1];
}

function getScannerAndBeaconInfo(): [[number, number, number][], {[_:string]: [number, number, number]}] {
    let input = getInput();

    let baseBeaconMap:[number, number, number][] = input['0'];

    let scannerOffsets: {[_: string]: [number, number, number]} = {};

    let mapIncomplete = true;
    let scannerIntegrated = ['0'];

    while (mapIncomplete) {
        mapIncomplete = false;

        for (let i in input) {
            if (i !== '0') {
                let scannerOrientation = getScannerOffsetAndOrientation(input[i], baseBeaconMap);

                if (scannerOrientation !== null && !scannerIntegrated.includes(i)) {
                    console.log("Integrating data from scanner " + i);
                    scannerIntegrated.push(i);

                    let orientedScannerField = orientScannerField(
                        input[i],
                        [scannerOrientation[0], scannerOrientation[1], scannerOrientation[2]],
                        [scannerOrientation[3], scannerOrientation[4], scannerOrientation[5]]
                    );

                    baseBeaconMap = [
                        ...new Set(
                            orientedScannerField
                                .reduce((acc: [number, number, number][], c: [number, number, number]) => [...acc, c], baseBeaconMap)
                                .map(c => c.join('|')))]
                        .map(c => c
                            .split('|')
                            .map(d => parseInt(d)));

                    scannerOffsets[i] = [scannerOrientation[0], scannerOrientation[1], scannerOrientation[2]];
                }

                if (!scannerIntegrated.includes(i)) {
                    mapIncomplete = true;
                }
            }
        }
    }

    return [baseBeaconMap, scannerOffsets];
}

function getScannerOffsetAndOrientation(
    scanner: number[][],
    baseScanner: number[][]
): [number, number, number, string, string, string] | null {
    const orientations = [
        ['x', 'y', 'z'], ['y', '-x', 'z'], ['-x', '-y', 'z'], ['-y', 'x', 'z'],
        ['y', 'x', '-z'], ['-x', 'y', '-z'], ['-y', '-x', '-z'], ['x', '-y', '-z'],
        ['-z', 'y', 'x'], ['-z', '-x', 'y'], ['-z', '-y', '-x'], ['-z', 'x', '-y'],
        ['z', 'x', 'y'], ['z', 'y', '-x'], ['z', '-x', '-y'], ['z', '-y', 'x'],
        ['y', 'z', 'x'], ['-x', 'z', 'y'], ['-y', 'z', '-x'], ['x', 'z', '-y'],
        ['x', '-z', 'y'], ['y', '-z', '-x'], ['-x', '-z', '-y'], ['-y', '-z', 'x']
    ];

    for (let orientation of orientations) {
        let offset = checkForBeaconOverlap(
            orientScannerField(
                scanner,
                [0, 0, 0],
                [orientation[0], orientation[1], orientation[2]]
            ),
            baseScanner
        );

        if (offset !== null) {
            return [...offset, orientation[0], orientation[1], orientation[2]];
        }
    }

    return null;
}

function orientScannerField(
    scanner: number[][],
    offset: [number, number, number],
    orientation: [string, string, string],
): number[][] {
    let orientedScanner = scanner.map(c => {
        const coordMap: { [_: string]: number } = {x: c[0], y: c[1], z: c[2]};
        const results = [...orientation];

        for (let coord in coordMap) {
            results[0] = results[0].replace(coord, coordMap[coord].toString()).replace('--', '');
            results[1] = results[1].replace(coord, coordMap[coord].toString()).replace('--', '');
            results[2] = results[2].replace(coord, coordMap[coord].toString()).replace('--', '');
        }

        return [parseInt(results[0]), parseInt(results[1]), parseInt(results[2])];
    })

    return orientedScanner.map(c => [c[0] + offset[0], c[1] + offset[1], c[2] + offset[2]]);
}

function checkForBeaconOverlap(coords: number[][], baseCoords: number[][]): [number, number, number] | null {
    for (let coord of coords) {
        for (let baseCoord of baseCoords) {
            let offset: [number, number, number] = [
                baseCoord[0] - coord[0],
                baseCoord[1] - coord[1],
                baseCoord[2] - coord[2]
            ];

            if (checkFor12BeaconsAtOffset(coords, baseCoords, offset)) {
                return offset;
            }
        }
    }

    return null;
}

function checkFor12BeaconsAtOffset(coords: number[][], baseCoords: number[][], offset: [number, number, number]): boolean {
    let beaconCount = 1;

    for (let coord of coords) {
        let offsetCoord = [coord[0] + offset[0], coord[1] + offset[1], coord[2] + offset[2]];

        for (let baseCoord of baseCoords) {
            if (baseCoord[0] === offsetCoord[0] && baseCoord[1] === offsetCoord[1] && baseCoord[2] === offsetCoord[2]) {
                beaconCount++;
            }

            if (beaconCount >= 12) {
                return true;
            }
        }
    }

    return false;
}

function getInput(): coords {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split('\n\n')
        .map(s => s
            .split("\n"))
        .reduce((acc: coords, s) => {
            acc[s[0].split(' ')[2]] = s
                .slice(1)
                .map(c => c
                    .split(',')
                    .map(d => parseInt(d)));

            return acc;
        }, {});
}

export {part1, part2};
