import fs from "fs";

interface Move {
    srcX: number,
    srcY: number,
    dstX: number,
    dstY: number
}

function part1() {
    const input = getInput();

    let lowestEnergy = getLowestEnergyToCompleteGame(input);

    console.log(lowestEnergy);
}

function part2() {
    let input = addExtraRows(getInput());

    let lowestEnergy = getLowestEnergyToCompleteGame(input);

    console.log(lowestEnergy);
}

function getLowestEnergyToCompleteGame(gameState: string[][]): number {
    if (isGameComplete(gameState)) {
        //We are complete - return with 0 cost
        return 0;
    }

    const energyCosts: { [_: string]: number } = {
        A: 1,
        B: 10,
        C: 100,
        D: 1000
    };

    let energyCostsOfMoves = getAvailableMoves(gameState).map(am =>
        energyCosts[gameState[am.srcY][am.srcX]] * (Math.abs(am.srcX - am.dstX) + Math.abs(am.srcY - am.dstY))
        + getLowestEnergyToCompleteGame(updateGameState(gameState, am)));

    return Math.min(...[...energyCostsOfMoves, Infinity]);
}

function getAvailableMoves(gameState: string[][]): Move[] {
    let availableMoves: Move[] = [];

    const burrowSpaces: { [_: string]: number } = {
        'A': 3,
        'B': 5,
        'C': 7,
        'D': 9
    };

    //Are there any amphipods in the hallway that can come home? Bring them home straight away
    for (let i = 1; i <= 11; i++) {
        if (gameState[1][i] !== '.') {
            //There is a piece in the hallway - is its burrow friendly?
            const piece = gameState[1][i];

            if (isBurrowFriendly(gameState, burrowSpaces[piece], piece)) {
                const dir = (burrowSpaces[piece] - i) / Math.abs(burrowSpaces[piece] - i);

                if (isCorridorClear(gameState, i + dir, burrowSpaces[piece])) {
                    //Work out how far down the target burrow we can go

                    let burrowDepth = 0;

                    for (let j = 2; j < gameState.length - 1; j++) {
                        if (gameState[j][burrowSpaces[piece]] === '.') {
                            burrowDepth = j;
                        }
                    }

                    //Start moving along corridor in the right direction
                    availableMoves.push({
                        srcX: i,
                        srcY: 1,
                        dstX: burrowSpaces[piece],
                        dstY: burrowDepth
                    });
                }
            }
        }
    }

    //Finally, can we bring any pieces out of their burrows?
    if (availableMoves.length === 0) {
        let incorrectBurrows = [];

        for (let burrowPiece of ['A', 'B', 'C', 'D']) {
            if (!isBurrowFriendly(gameState, burrowSpaces[burrowPiece], burrowPiece)) {
                incorrectBurrows.push(burrowSpaces[burrowPiece]);
            }
        }

        for (let incorrectBurrowSpace of [...new Set(incorrectBurrows)]) {
            //Find first piece in burrow that must move out
            let pieceToMove = gameState.length - 2;
            let possibleDestinationSpaces = [];

            for (let i = gameState.length - 2; i >= 2; i--) {
                if (gameState[i][incorrectBurrowSpace] !== '.') {
                    pieceToMove = i;
                }
            }

            for (let destinationSpace of [1, 2, 4, 6, 8, 10, 11]) {
                if (isCorridorClear(gameState, incorrectBurrowSpace, destinationSpace)) {
                    possibleDestinationSpaces.push(destinationSpace);
                }
            }

            for (let possibleDestinationSpace of possibleDestinationSpaces) {
                availableMoves.push(
                    {
                        srcX: incorrectBurrowSpace,
                        srcY: pieceToMove,
                        dstX: possibleDestinationSpace,
                        dstY: 1
                    }
                );
            }
        }
    }

    return availableMoves;
}

function isGameComplete(gameState: string[][]) {
    for (let i = 2; i < gameState.length - 1; i++) {
        if (gameState[i][3] !== 'A') {
            return false;
        }

        if (gameState[i][5] !== 'B') {
            return false;
        }

        if (gameState[i][7] !== 'C') {
            return false;
        }

        if (gameState[i][9] !== 'D') {
            return false;
        }
    }

    return true;
}

function isCorridorClear(gameState: string[][], x1: number, x2: number): boolean {
    for (let i = Math.min(x1, x2); i <= Math.max(x1, x2); i++) {
        if (gameState[1][i] !== '.') {
            return false;
        }
    }

    return true;
}

function isBurrowFriendly(gameState: string[][], burrowXCoord: number, friendlyType: string): boolean {
    for (let y = 2; y < gameState.length - 1; y++) {
        if (gameState[y][burrowXCoord] !== friendlyType && gameState[y][burrowXCoord] !== '.') {
            return false;
        }
    }

    return true;
}

function updateGameState(gameState: string[][], move: Move): string[][] {
    let newGameState: string[][] = [];

    for (let i = 0; i < gameState.length; i++) {
        newGameState[i] = [];

        for (let j = 0; j < gameState[i].length; j++) {
            newGameState[i][j] = gameState[i][j];
        }
    }

    newGameState[move.dstY][move.dstX] = gameState[move.srcY][move.srcX];
    newGameState[move.srcY][move.srcX] = '.';

    return newGameState;
}

function getInput(): string[][] {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split('\n')
        .map(line => line.split(''));
}

function addExtraRows(input: string[][]) {
    let insertionRows = [
        '  #D#C#B#A#  ',
        '  #D#B#A#C#  '
    ];

    let newInput: string[][] = [];

    for (let i = 0; i <= 2; i++) {
        newInput[i] = [];

        for (let j = 0; j < input[i].length; j++) {
            newInput[i][j] = input[i][j];
        }
    }

    for (let i = 0; i < 2; i++) {
        newInput[i + 3] = [];

        for (let j = 0; j < input[i].length; j++) {
            newInput[i + 3][j] = insertionRows[i][j];
        }
    }

    for (let i = 3; i < input.length; i++) {
        newInput[i + 2] = [];

        for (let j = 0; j < input[i].length; j++) {
            newInput[i + 2][j] = input[i][j];
        }
    }

    return newInput;
}

export {part1, part2};
