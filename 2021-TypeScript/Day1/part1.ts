import * as fs from 'fs';

export default function (filename: string) {
    const counter = calculateLargerMeasurements(
        getInput(filename)
    );

    console.log(counter + " measurements are larger than the previous measurement");
}

function getInput(filename: string) {
    return fs
        .readFileSync(filename, 'utf8')
        .split("\n")
        .map(l => parseInt(l));
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
