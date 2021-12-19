import * as fs from 'fs';

function part1() {
    let input = getInput();

    let sum = input
        .slice(1)
        .reduce((sum, curr) => addPairs(sum, curr), input[0]);

    console.log("Magnitude of pair sum: " + getPairMagnitude(sum));
}

function part2() {
    let input = getInput();

    let largestMagnitude = 0;

    for (let i in input) {
        for (let j in input) {
            if (i !== j) {
                let pairMagnitude = getPairMagnitude(addPairs(input[i], input[j]));

                if (pairMagnitude > largestMagnitude) {
                    largestMagnitude = pairMagnitude;
                }
            }
        }
    }

    console.log("Largest magnitude is " + largestMagnitude);
}

function addPairs(pair1: string, pair2: string): string {
    return reducePair('[' + pair1 + ',' + pair2 + ']');
}

function reducePair(pair: string): string {
    let stillGoing = true, oldPairPrint = '', newPairPrint = '';
    let originalPairPrint = pair;

    while (stillGoing) {
        stillGoing = false;

        oldPairPrint = pair;
        pair = explodeFirst4DeepPair(pair);
        newPairPrint = pair;

        if (newPairPrint !== oldPairPrint) {
            stillGoing = true;
        }
    }

    pair = splitFirstNumberHigherThan10(pair);

    if (pair !== originalPairPrint) {
        pair = reducePair(pair);
    }

    return pair;
}

function explodeFirst4DeepPair(pair: string): string {
    let depth = 0, lastNumberStart = 0, lastNumberEnd = 0, lastNumber = 0;
    let outputString = '';

    for (let i = 0; i < pair.length; i++ ) {
        if (pair[i] == '[') {
            depth++;
        }

        if (pair[i] == ']') {
            depth--;
        }

        let numberRegex = /^\d+/;

        if (i >= lastNumberEnd && numberRegex.test(pair.slice(i))) {
            let matches = pair.slice(i).match(numberRegex);

            if (matches !== null) {
                lastNumberStart = i;
                lastNumberEnd = lastNumberStart + matches[0].length;
                lastNumber = parseInt(matches[0]);
            }
        }

        let pairRegex = /^\[(\d+),(\d+)\]/;

        if (depth > 4 && pairRegex.test(pair.slice(i))) {
            let matches = pair.slice(i).match(pairRegex);

            if (matches !== null) {
                let pairLength = matches[0].length;
                let leftNumber = parseInt(matches[1]);
                let rightNumber = parseInt(matches[2]);

                if (lastNumberEnd !== 0) {
                    outputString = pair.slice(0, lastNumberStart);
                    outputString += (lastNumber + leftNumber).toString();
                    outputString += pair.slice(lastNumberEnd, i);
                } else {
                    outputString = pair.slice(0, i);
                }

                outputString += '0';

                for (let j = i + pairLength; j < pair.length; j++) {
                    if (numberRegex.test(pair.slice(j))) {
                        let matches = pair.slice(j).match(numberRegex);

                        if (matches !== null) {
                            let nextNumberLength = matches[0].length;
                            let nextNumber = parseInt(matches[0]);

                            outputString += pair.slice(i + pairLength, j);
                            outputString += (nextNumber + rightNumber).toString();
                            outputString += pair.slice(j + nextNumberLength);

                            return outputString;
                        }
                    }
                }

                outputString += pair.slice(i + pairLength);

                return outputString;
            }
        }
    }

    return pair;
}

function splitFirstNumberHigherThan10 (pair: string): string {
    let numberRegex = /^\d+/;

    for (let i = 0; i < pair.length; i++ ) {
        if (numberRegex.test(pair.slice(i))) {
            let matches = pair.slice(i).match(numberRegex);

            if (matches !== null) {
                let number = parseInt(matches[0]);
                let numberLength = matches[0].length;

                if (number >= 10) {
                    let outputString = pair.slice(0, i);
                    outputString += '[' + Math.floor(number / 2) + ',' + Math.ceil(number / 2)  + ']';
                    outputString += pair.slice(i + numberLength);

                    return outputString;
                }
            }
        }
    }

    return pair;
}

function getPairMagnitude(pair: string): number {
    let pairRegex = /^\[(\d+),(\d+)\]/;

    if (parseInt(pair).toString() === pair) {
        return parseInt(pair);
    }

    for (let i = 0; i < pair.length; i++ ) {
        if (pairRegex.test(pair.slice(i))) {
            let matches = pair.slice(i).match(pairRegex);

            if (matches !== null) {
                let pairMagnitude = 3 * parseInt(matches[1]) + 2 * parseInt(matches[2]);

                return getPairMagnitude(pair.slice(0, i) + pairMagnitude + pair.slice(i + matches[0].length));
            }
        }
    }

    return 0;
}

function getInput(): string[] {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split('\n');
}

export {part1, part2};
