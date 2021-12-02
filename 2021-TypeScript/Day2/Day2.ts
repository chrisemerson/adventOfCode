import * as fs from 'fs';

interface Position {
    forward: number,
    depthPt1: number,
    depthPt2: number
}

interface Instruction {
    direction: string,
    value: number
}

function part1 () {
    const position = runInstructions(getInput());

    outputAnswer(1, position.depthPt1, position.forward);
}

function part2 () {
    const position = runInstructions(getInput());

    outputAnswer(2, position.depthPt2, position.forward);
}

function runInstructions(input: Instruction[]): Position {
    let depthPt1 = 0;
    let depthPt2 = 0;
    let forward = 0;
    let aim = 0;

    for (let instruction of input) {
        switch (instruction.direction) {
            case 'up':
                depthPt1 -= instruction.value;
                aim -= instruction.value;
                break;

            case 'down':
                depthPt1 += instruction.value;
                aim += instruction.value;
                break;

            case 'forward':
                forward += instruction.value;
                depthPt2 += (aim * instruction.value);
                break;
        }
    }

    return {
        forward,
        depthPt1,
        depthPt2
    }
}

function getInput(): Instruction[] {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .split("\n")
        .filter(l => l !== '')
        .map(l => l.split(" "))
        .map(parts => ({direction: parts[0], value: parseInt(parts[1])}));
}

function outputAnswer(partNo: number, depth: number, forward: number) {
    console.log(
        'Part ' + partNo + ': Depth is '
        + depth
        + ', forward distance is '
        + forward
        + ', multiplication is '
        + (depth * forward)
    );
}

export { part1, part2 };
