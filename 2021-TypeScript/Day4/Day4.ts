import * as fs from 'fs';

interface BingoGame {
    numbers: number[],
    cards: BingoCard[],
    lastNumber: number
}

interface BingoCard {
    [_: number]: (number|string)[]
}

function part1 () {
    let game = getInput();

    while (true) {
        game = processNextBingoNumber(game);

        if (checkForWinningCards(game)) {
            break;
        }
    }
}

function processNextBingoNumber(game: BingoGame): BingoGame {
    return {
        numbers: game.numbers.slice(1),
        cards: game.cards.map(c => markNumberOnCard(game.numbers[0], c)),
        lastNumber: game.numbers[0]
    }
}

function checkForWinningCards(game: BingoGame): boolean {
    let winningCard = false;

    game.cards.forEach(c => {
        let isWinningCard = checkForWinningCard(c, game.lastNumber);
        winningCard = winningCard || isWinningCard;

        return isWinningCard
    });

    return winningCard;
}

function checkForWinningCard(card: BingoCard, lastNumber: number): boolean {
    let winningCard = false;

    Object.values(card).forEach(c => {
        if (c.filter((v: number|string) => v === 'X').length === 5) {
            winningCard = true;
        }
    });

    for (let i = 0; i < 5; i ++) {
        let winningCardThisCol = true;

        Object.values(card).forEach(r => {
            if (r[i] !== 'X') {
                winningCardThisCol = false;
            }
        });

        if (winningCardThisCol) {
            winningCard = true;
        }
    }

    if (winningCard) {
        let sumOfUnmarkedNumbers = Object.values(card)
            .map(r => r
                .filter((v: number|string) => v !== 'X')
                .reduce((a: number, b: number) => a + b, 0))
            .reduce((a, b) => a + b, 0);

        console.log("Winning card's score is " + (sumOfUnmarkedNumbers * lastNumber));
    }

    return winningCard;
}

function markNumberOnCard(number: number, card: BingoCard): BingoCard {
    return Object.values(card).map(r => r.map((c: number|string) => {
        return c === number ? 'X' : c;
    }));
}

function part2 () {
    getInput();
}

function getInput(): BingoGame {
    const input = fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .split("\n\n")
        .filter(l => l !== '');

    return {
        numbers: input[0].split(',').map(s => parseInt(s)),
        cards: input.slice(1).map(
            c => c
                .split("\n")
                .filter(l => l !== '')
                .map(l => l
                    .split(' ')
                    .filter(n => n !== '')
                    .map(n => parseInt(n))
                )
        ),
        lastNumber: 0
    };
}

export { part1, part2 };
