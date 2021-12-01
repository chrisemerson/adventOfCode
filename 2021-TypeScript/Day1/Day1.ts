import * as fs from 'fs';

function part1 (filename: string) {
    const counter = calculateLargerMeasurements(
        getInput(filename)
    );

    console.log(
        counter + " measurements are larger than the previous measurement"
    );
}

function part2 (filename: string) {
    const counter = calculateLargerMeasurements(
        sumArrayPartitions(
            partitionArrayWithSlidingWindow(
                getInput(filename),
                3
            )
        )
    );

    console.log(
        counter
        + " measurements are larger than the previous measurement"
        + " using a sliding window"
    );
}

function calculateLargerMeasurements(input: number[]) {
    let previousValue: number | null = null, counter = 0;

    for (let value of input) {
        if (previousValue !== null && value > previousValue) {
            counter++;
        }

        previousValue = value;
    }

    return counter;
}

function partitionArrayWithSlidingWindow(
    array: number[],
    partitionSize: number
): number[][] {
    let partitionedArray: number[][] = [];

    for (let i = 0; i + partitionSize <= array.length; i++) {
        let thisPartition = [];

        for (let j = 0; j < partitionSize; j++) {
            thisPartition.push(array[i + j]);
        }

        partitionedArray.push(thisPartition);
    }

    return partitionedArray;
}

function sumArrayPartitions(array: number[][]): number[] {
    let arrayOfSums: number[] = [];

    for (let window of array) {
        arrayOfSums.push(
            window.reduce(
                (sum, current) => sum + current,
                0
            )
        );
    }

    return arrayOfSums;
}

function getInput(filename: string) {
    return fs
        .readFileSync(filename, 'utf8')
        .split("\n")
        .map(l => parseInt(l));
}

export { part1, part2 };
