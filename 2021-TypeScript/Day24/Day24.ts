const part1 = () => console.log("The highest valid model number is " + findHighestInputThatGivesCorrectOutput());
const part2 = () => console.log("The lowest valid model number is " + findLowestInputThatGivesCorrectOutput());

function findHighestInputThatGivesCorrectOutput(): number {
    for (let a = 1; a >= 1; a--) {
        for (let b = 9; b >= 1; b--) {
            for (let cd = 9; cd >= 1; cd--) {
                for (let e = 9; e >= 1; e--) {
                    for (let f = 9; f >= 1; f--) {
                        for (let gh = 9; gh >= 2; gh--) {
                            for (let ij = 7; ij >= 1; ij--) {
                                for (let k = 9; k >= 1; k--) {
                                    for (let l = 9; l >= 1; l--) {
                                        const input = constructInput(a, b, cd, e, f, gh, ij, k, l);
                                        const programResult = runProgram(input);
                                        const validationResult = validateResult(input, programResult);

                                        if (validationResult !== false) {
                                            return validationResult;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

function findLowestInputThatGivesCorrectOutput(): number {
    for (let a = 1; a <= 9; a++) {
        for (let b = 1; b <= 9; b++) {
            for (let cd = 1; cd <= 9; cd++) {
                for (let e = 1; e <= 9; e++) {
                    for (let f = 1; f <= 9; f++) {
                        for (let gh = 2; gh <= 9; gh++) {
                            for (let ij = 1; ij <= 7; ij++) {
                                for (let k = 1; k <= 9; k++) {
                                    for (let l = 1; l <= 9; l++) {
                                        const input = constructInput(a, b, cd, e, f, gh, ij, k, l);
                                        const programResult = runProgram(input);
                                        const validationResult = validateResult(input, programResult);

                                        if (validationResult !== false) {
                                            return validationResult;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

function constructInput(a, b, cd, e, f, gh, ij, k, l): number {
    return a * 100000000000
        + b * 10000000000
        + cd * 1000000000
        + cd * 100000000
        + e * 10000000
        + f * 1000000
        + gh * 100000
        + (gh - 1) * 10000
        + ij * 1000
        + (ij + 2) * 100
        + k * 10
        + l;
}

function validateResult(input: number, result: number): number|false {
    const validOutcomes: {[_: number]: number[]} = {
        1: [173, 199, 225, 251, 277, 303, 329, 355, 381, 407],
        2: [174, 200, 226, 252, 278, 304, 330, 356, 382, 408],
        3: [175, 201, 227, 253, 279, 305, 331, 357, 383, 409],
        4: [176, 202, 228, 254, 280, 306, 332, 358, 384, 410],
        5: [177, 203, 229, 255, 281, 307, 333, 359, 385, 411],
        6: [178, 204, 230, 256, 282, 308, 334, 360, 386, 412],
        7: [179, 205, 231, 257, 283, 309, 335, 361, 387, 413],
        8: [180, 206, 232, 258, 284, 310, 336, 362, 388, 414],
        9: [181, 207, 233, 259, 285, 311, 337, 363, 389, 415]
    };

    for (let nextInput in validOutcomes) {
        if (validOutcomes[nextInput].includes(result)) {
            const finalDigit = Math.floor(result / 26) - 6;

            if (finalDigit === 0) {
                return false;
            } else {
                return parseInt(input.toString() + nextInput.toString() + finalDigit.toString());
            }
        }
    }

    return false;
}

function runProgram(input: number): number {
    const inputDigits = input.toString().split('').map(d => parseInt(d));

    // In every if statement, we must choose the 'then' branch
    // We multiply by 26 6 times without choice in this algorithm, and there are only 7 chances to divide by 26 - we
    // must use all of them to have any chance of z being 0 at the end

    let z = inputDigits[0] + 12;
    z = z * 26 + inputDigits[1] + 9;
    z = z * 26 + inputDigits[2] + 8;

    // 4th digit must be same as 3rd - the - 8 here cancels out the + 8 above and the input digits are the only
    // remainder left after dividing by 26 again

    if (z % 26 - 8 === inputDigits[3]) {
        z = Math.trunc(z / 26);
    } else {
        z = Math.trunc(z / 26) * 26 + inputDigits[3] + 3;
    }

    z = z * 26 + inputDigits[4];
    z = z * 26 + inputDigits[5] + 11;
    z = z * 26 + inputDigits[6] + 10;

    //8th digit must be 1 less than 7th

    if (z % 26 - 11 === inputDigits[7]) {
        z = Math.trunc(z / 26);
    } else {
        z = Math.trunc(z / 26) * 26 + inputDigits[7] + 13;
    }

    z = z * 26 + inputDigits[8] + 3;

    //10th digit must be 2 more than 9th

    if (z % 26 - 1 === inputDigits[9]) {
        z = Math.trunc(z / 26);
    } else {
        z = Math.trunc(z / 26) * 26 + inputDigits[9] + 10;
    }

    if (z % 26 - 8 === inputDigits[10]) {
        z = Math.trunc(z / 26);
    } else {
        z = Math.trunc(z / 26) * 26 + inputDigits[10] + 10;
    }

    if (z % 26 - 5 === inputDigits[11]) {
        z = Math.trunc(z / 26);
    } else {
        z = Math.trunc(z / 26) * 26 + inputDigits[11] + 14;
    }

    // z must be 1 of 100 different values at this point to be a valid input (see validateResult function) and which one
    // it is determines the last 2 digits

    return z;
}

export {part1, part2};
