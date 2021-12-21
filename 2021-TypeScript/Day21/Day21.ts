function part1() {
    let spaces = [7, 6];
    let scores = [0, 0];

    let turn = 0, dice = 1, diceRolls = 0;

    while (scores[0] < 1000 && scores[1] < 1000) {
        let roll = dice;

        dice = (dice + 1) % 100;
        roll += dice;
        dice = (dice + 1) % 100;
        roll += dice;
        dice = (dice + 1) % 100;

        spaces[turn] = (spaces[turn] + roll - 1) % 10 + 1;
        scores[turn] += spaces[turn];

        diceRolls += 3;
        turn = 1 - turn;
    }

    console.log("Player " + (turn + 1) + " lost the game with a score of " + scores[turn] + " after " + diceRolls + " dice rolls.");
    console.log("Product is " + (diceRolls * scores[turn]));
}

function part2() {
    let gameResults = playGameWithQuantumDice(7, 6, 0, 0);

    console.log("The first player wins in " + gameResults[0]  + " universes and loses in " + gameResults[1] + " universes");
}

function playGameWithQuantumDice(nextPlayerSpace: number, lastPlayerSpace: number, nextPlayerScore: number, lastPlayerScore: number): [number, number] {
    if (lastPlayerScore >= 21) {
        return [0, 1];
    }

    let gamesWonByNextPlayer = 0, gamesWonByLastPlayer = 0, gamesWonNP = 0, gamesWonLP = 0;

    // There is only 1 way to roll 3 with 3 dice - [1 1 1]
    nextPlayerSpace = ((nextPlayerSpace + 2) % 10) + 1;
    [gamesWonNP, gamesWonLP] = playGameWithQuantumDice(lastPlayerSpace, nextPlayerSpace, lastPlayerScore, (nextPlayerScore + nextPlayerSpace));
    gamesWonByNextPlayer += gamesWonNP;
    gamesWonByLastPlayer += gamesWonLP;

    // There are 3 ways to roll 4 - [1 1 2, 1 2 1, 2 1 1]
    nextPlayerSpace = (nextPlayerSpace % 10) + 1;
    [gamesWonNP, gamesWonLP] = playGameWithQuantumDice(lastPlayerSpace, nextPlayerSpace, lastPlayerScore, (nextPlayerScore + nextPlayerSpace));
    gamesWonByNextPlayer += 3 * gamesWonNP;
    gamesWonByLastPlayer += 3 * gamesWonLP;

    // There are 6 ways to roll 5 - [1 1 3, 1 2 2, 1 3 1, 2 1 2, 2 2 1, 3 1 1]
    nextPlayerSpace = (nextPlayerSpace % 10) + 1;
    [gamesWonNP, gamesWonLP] = playGameWithQuantumDice(lastPlayerSpace, nextPlayerSpace, lastPlayerScore, (nextPlayerScore + nextPlayerSpace));
    gamesWonByNextPlayer += 6 * gamesWonNP;
    gamesWonByLastPlayer += 6 * gamesWonLP;

    // There are 7 ways to roll 6 - [1 2 3, 1 3 2, 2 1 3, 2 2 2, 2 3 1, 3 1 2, 3 2 1]
    nextPlayerSpace = (nextPlayerSpace % 10) + 1;
    [gamesWonNP, gamesWonLP] = playGameWithQuantumDice(lastPlayerSpace, nextPlayerSpace, lastPlayerScore, (nextPlayerScore + nextPlayerSpace));
    gamesWonByNextPlayer += 7 * gamesWonNP;
    gamesWonByLastPlayer += 7 * gamesWonLP;

    // There are 6 ways to roll 7 - [1 3 3, 2 2 3, 2 3 2, 3 1 3, 3 2 2, 3 3 1]
    nextPlayerSpace = (nextPlayerSpace % 10) + 1;
    [gamesWonNP, gamesWonLP] = playGameWithQuantumDice(lastPlayerSpace, nextPlayerSpace, lastPlayerScore, (nextPlayerScore + nextPlayerSpace));
    gamesWonByNextPlayer += 6 * gamesWonNP;
    gamesWonByLastPlayer += 6 * gamesWonLP;


    // There are 3 ways to roll 8 - [1 1 2, 1 2 1, 2 1 1]
    nextPlayerSpace = (nextPlayerSpace % 10) + 1;
    [gamesWonNP, gamesWonLP] = playGameWithQuantumDice(lastPlayerSpace, nextPlayerSpace, lastPlayerScore, (nextPlayerScore + nextPlayerSpace));
    gamesWonByNextPlayer += 3 * gamesWonNP;
    gamesWonByLastPlayer += 3 * gamesWonLP;

    // There is only 1 way to roll 9 - [3 3 3]
    nextPlayerSpace = (nextPlayerSpace % 10) + 1;
    [gamesWonNP, gamesWonLP] = playGameWithQuantumDice(lastPlayerSpace, nextPlayerSpace, lastPlayerScore, (nextPlayerScore + nextPlayerSpace));
    gamesWonByNextPlayer += gamesWonNP;
    gamesWonByLastPlayer += gamesWonLP;

    return [gamesWonByLastPlayer, gamesWonByNextPlayer];
}


export {part1, part2};
