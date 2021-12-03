import * as fs from 'fs';

function part1 () {
    const digitsByPos = transposeDigits(getInput());

    const gammaRate = parseInt(
        digitsByPos.map(findMostCommonDigit).join(''),
        2
    );

    const epsilonRate = parseInt(
        digitsByPos.map(findLeastCommonDigit).join(''),
        2
    );

    console.log("Part 1: Power consumption is " + (gammaRate * epsilonRate));
}

function part2 () {
    let inputOxygenGeneratorRating = getInput();
    let inputCO2ScrubberRating = getInput();

    for (let i = 0; i < inputOxygenGeneratorRating[0].length && inputOxygenGeneratorRating.length > 1; i++) {
        const digitsByPos = transposeDigits(inputOxygenGeneratorRating);
        inputOxygenGeneratorRating = inputOxygenGeneratorRating.filter(
            l => l[i] === findMostCommonDigit(digitsByPos[i])
        );
    }

    for (let i = 0; i < inputCO2ScrubberRating[0].length && inputCO2ScrubberRating.length > 1; i++) {
        const digitsByPos = transposeDigits(inputCO2ScrubberRating);
        inputCO2ScrubberRating = inputCO2ScrubberRating.filter(
            l => l[i] === findLeastCommonDigit(digitsByPos[i])
        );
    }

    const finalOxygenGeneratorRating = parseInt(inputOxygenGeneratorRating[0], 2);
    const finalCO2ScrubberRating = parseInt(inputCO2ScrubberRating[0], 2);

    console.log("Part 2: Life support rating is " + (finalOxygenGeneratorRating * finalCO2ScrubberRating));
}

function transposeDigits(input: string[]) {
    let digitsByPos: string[] = [];

    for (let i = 0; i < input[0].length; i++) {
        digitsByPos[i] = '';

        input.forEach(line => {
            digitsByPos[i] += line[i];
        });
    }
    return digitsByPos;
}

function findMostCommonDigit(input: string) {
    const frequencies = getDigitFrequencies(input);

    return frequencies['0'] > frequencies['1'] ? '0' : '1';
}

function findLeastCommonDigit(input: string) {
    const frequencies = getDigitFrequencies(input);

    return frequencies['1'] < frequencies['0'] ? '1' : '0';
}

function getDigitFrequencies(input: string) {
    let charFrequencies: { [_: string]: number } = {};

    for (let digit of input) {
        let thisDigit = digit as keyof typeof charFrequencies;

        if (!charFrequencies[thisDigit]) {
            charFrequencies[thisDigit] = 0;
        }

        charFrequencies[thisDigit]++;
    }

    return charFrequencies;
}

function getInput(): string[] {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .split("\n")
        .filter(l => l !== '');
}

export { part1, part2 };
