import * as fs from 'fs';

type PopulationDistribution = {[_: number]: number};

function part1 () {
    runFishSimulation(
        getFishPopulationDistribution(getInput()),
        80
    );
}

function part2 () {
    runFishSimulation(
        getFishPopulationDistribution(getInput()),
        256
    );
}

function runFishSimulation(fish: PopulationDistribution, days: number) {
    for (let i = 0; i < days; i++) {
        fish = iterateFish(fish);
    }

    const totalFish = Object.values(fish).reduce((a, f) => a + f, 0);

    console.log("There are " + totalFish + " fish after " + days + " days");
}

function iterateFish(fish: PopulationDistribution): PopulationDistribution {
    let newDistribution:PopulationDistribution = {}

    for (let day = 0; day < 8; day++) {
        if (fish[day + 1]) {
            newDistribution[day] = fish[day + 1];
        } else {
            newDistribution[day] = 0;
        }
    }

    newDistribution[8] = fish[0] ? fish[0] : 0;
    newDistribution[6] += newDistribution[8];

    return newDistribution;
}

function getFishPopulationDistribution(fish: number[]): PopulationDistribution {
    let populationDistribution:PopulationDistribution = {};

    for (const f of fish) {
        if (!populationDistribution[f]) {
            populationDistribution[f] = 0;
        }

        populationDistribution[f]++;
    }

    return populationDistribution;
}

function getInput(): number[] {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split(',')
        .map(f => parseInt(f))
}

export { part1, part2 };
