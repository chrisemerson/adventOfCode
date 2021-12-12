import * as fs from 'fs';
import {getAllEdgeLocations} from "../Utils/grid";

function part1() {
    let energyLevels = getInput();
    let flashes = 0;
    let totalFlashes = 0;
    const steps = 100;

    for (let i = 0; i < steps; i++) {
        [energyLevels, flashes] = stepEnergyLevels(energyLevels);
        totalFlashes += flashes;
    }

    console.log("Total number of flashes: " + totalFlashes);
}

function part2() {
    let energyLevels = getInput();
    let flashes = 0;
    let step = 0;

    while (flashes < 100) {
        [energyLevels, flashes] = stepEnergyLevels(energyLevels);
        step++;
    }

    console.log("Total number of steps before all flashes: " + step);
}

function stepEnergyLevels(energyLevels: number[][]): [number[][], number] {
    let newEnergyLevels = propagateEnergyLevels(energyLevels.map(row => row.map(o => o === 9 ? -1 : o + 1)));

    const flashes = newEnergyLevels
        .map(r => r
            .filter(d => d === -2)
            .length
        )
        .reduce((acc, x) => acc + x, 0);

    newEnergyLevels = newEnergyLevels
        .map(r => r.map(d => d === -2 ? 0 : d));

    return [newEnergyLevels, flashes];
}

function propagateEnergyLevels(energyLevels: number[][]): number[][] {
    let stillPropagating = true;

    while (stillPropagating) {
        //Find edge locations of all cells that have flashed (value -1)
        let edgeLocations: number[][] = [];

        for (let i = 0; i < energyLevels.length; i++) {
            for (let j = 0; j < energyLevels[i].length; j++) {
                if (energyLevels[i][j] === -1) {
                    edgeLocations = edgeLocations.concat(getAllEdgeLocations(energyLevels, i, j));
                }
            }
        }

        //Change all -1 octopuses to -2, to indicate that they have flashed & propagated
        energyLevels = energyLevels
            .map(r => r.map(d => d === -1 ? -2 : d));

        //Propagate the flashes to all the neighbours of octopuses that flashed already
        for (let [i, j] of edgeLocations) {
            //Only increase the energy of octopuses that haven't already flashed, or are in the process of doing so
            if (energyLevels[i][j] >= 0) {
                energyLevels[i][j] += 1;
            }

            //If the octopus has increased to energy > 9, flash it!
            if (energyLevels[i][j] > 9) {
                energyLevels[i][j] = -1;
            }
        }

        //Count the number of octopuses that have flashed this step - if any, continue propagating
        if (energyLevels
            .map(r => r.filter(e => e === -1).length)
            .reduce((acc, x) => acc + x, 0) === 0) {
            stillPropagating = false;
        }
    }

    return energyLevels;
}

function getInput(): number[][] {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split("\n")
        .map(l => l
            .split('')
            .map(n => parseInt(n))
        );
}

export {part1, part2};
