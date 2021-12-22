import fs from "fs";

interface Region {
    x1: number,
    x2: number,
    y1: number,
    y2: number,
    z1: number,
    z2: number
}

function part1() {
    let input = getInput();
    let reactor = getReactor();

    for (let [onOff, cuboid] of input) {
        for (let x = Math.max(-50, cuboid.x1); x <= Math.min(50, cuboid.x2); x++) {
            for (let y = Math.max(-50, cuboid.y1); y <= Math.min(50, cuboid.y2); y++) {
                for (let z = Math.max(-50, cuboid.z1); z <= Math.min(50, cuboid.z2); z++) {
                    reactor[x][y][z] = (onOff == "on");
                }
            }
        }
    }

    let cubesOn = 0;

    for (let x = -50; x <= 50; x++) {
        for (let y = -50; y <= 50; y++) {
            for (let z = -50; z <= 50; z++) {
                if (reactor[x][y][z]) {
                    cubesOn++;
                }
            }
        }
    }

    console.log("Number of cubes on in reactor initialisation volume: " + cubesOn);
}

function part2() {
    const input = getInput();

    let cubesOn = 0;

    for (let i in input) {
        let [onOff, region] = input[i];

        if (onOff === "on") {
            cubesOn += getRegionSize(region) - countCubesInRegions(
                getOverlappingRegions(
                    region,
                    input
                        .slice(parseInt(i) + 1)
                        .map(l => l[1])
                )
            );
        }
    }

    console.log("Total number of cubes turned on in reactor: " + cubesOn);
}

function getRegionSize(region: Region): number {
    return (region.x2 - region.x1 + 1)
    * (region.y2 - region.y1 + 1)
    * (region.z2 - region.z1 + 1);
}

function countCubesInRegions(regions: Region[]): number {
    let cubes = 0;

    for (let i in regions) {
        let region = regions[i];

        cubes += getRegionSize(region) - countCubesInRegions(getOverlappingRegions(region, regions.slice(parseInt(i) + 1)));
    }

    return cubes;
}

function mapInputCoordsToRegion(coords: {[_: string]: [number, number]}): Region {
    return {
        x1: coords.x[0],
        x2: coords.x[1],
        y1: coords.y[0],
        y2: coords.y[1],
        z1: coords.z[0],
        z2: coords.z[1],
    }
}

function getOverlappingRegions(region: Region, regions: Region[]): Region[] {
    let overlapRegions: Region[] = [];

    for (let subRegion of regions) {
        if (
            region.x2 >= subRegion.x1
            && region.x1 <= subRegion.x2
            && region.y2 >= subRegion.y1
            && region.y1 <= subRegion.y2
            && region.z2 >= subRegion.z1
            && region.z1 <= subRegion.z2
        ) {
            let xCoords = [region.x1, region.x2, subRegion.x1, subRegion.x2]
                .sort((a, b) => a - b);

            let yCoords = [region.y1, region.y2, subRegion.y1, subRegion.y2]
                .sort((a, b) => a - b);

            let zCoords = [region.z1, region.z2, subRegion.z1, subRegion.z2]
                .sort((a, b) => a - b);

            overlapRegions.push({
                x1: xCoords[1],
                x2: xCoords[2],
                y1: yCoords[1],
                y2: yCoords[2],
                z1: zCoords[1],
                z2: zCoords[2]
            });
        }
    }

    return overlapRegions;
}

function getReactor(): boolean[][][] {
    let reactor: boolean[][][] = [];

    for (let x = -50; x <= 50; x++) {
        reactor[x] = [];

        for (let y = -50; y <= 50; y++) {
            reactor[x][y] = [];

            for (let z = -50; z <= 50; z++) {
                reactor[x][y][z] = false;
            }
        }
    }

    return reactor;
}

function getInput(): [string, Region][] {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split('\n')
        .map(line => line.split(' '))
        .map(line => {
            const ranges = line[1]
                .split(',')
                .map(dim => dim.split('='))
                .reduce((acc, c) => ({...acc, [c[0]]: c[1].split('..').map(d => parseInt(d))}), {});

            return [line[0], mapInputCoordsToRegion(ranges)];
        });
}

export {part1, part2};
