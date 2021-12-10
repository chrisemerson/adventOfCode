import * as fs from 'fs';

function part1() {
    let score = getInput()
        .map(l => findLineCompletionDetails(l)[0])
        .reduce((acc, x) => acc + x, 0);

    console.log("Syntax Error Score: " + score);
}

function part2() {
    let sortedCompletionScores = getInput()
        .filter(l => findLineCompletionDetails(l)[0] === 0)
        .map(l => findLineCompletionDetails(l)[1])
        .map(bs => findScoreOfBracketStack(bs))
        .sort((a, b) => a - b);

    console.log("Middle Completion Score: " + sortedCompletionScores[(sortedCompletionScores.length - 1) / 2]);
}

function findLineCompletionDetails(line: string): [number, string[]] {
    let bracketStack: string[] = [];

    for (let char of line) {
        switch (char) {
            case '(':
                bracketStack.push(')');
                break;

            case '[':
                bracketStack.push(']');
                break;

            case '{':
                bracketStack.push('}');
                break;

            case '<':
                bracketStack.push('>');
                break;

            default:
                if (bracketStack.pop() !== char) {
                    return [findBracketScorePt1(char), bracketStack];
                }

                break;
        }
    }

    return [0, bracketStack];
}

function findScoreOfBracketStack(stack: string[]): number {
    return stack
        .reverse()
        .reduce((acc, x) => (acc * 5) + findBracketScorePt2(x), 0)
}

function findBracketScorePt1(bracket: string): number {
    switch (bracket) {
        case ')': return 3;
        case ']': return 57;
        case '}': return 1197;
        case '>': return 25137;
    }

    return 0;
}

function findBracketScorePt2(bracket: string) {
    switch (bracket) {
        case ')': return 1;
        case ']': return 2;
        case '}': return 3;
        case '>': return 4;
    }

    return 0;
}

function getInput(): string[] {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split("\n");
}

export {part1, part2};
