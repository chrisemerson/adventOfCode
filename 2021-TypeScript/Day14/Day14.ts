import * as fs from 'fs';

type InsertionRules = {[_: string]: string}
type PairCount = {[_: string]: number}

const part1 = () => runPolymerisation(10);
const part2 = () => runPolymerisation(40);

function runPolymerisation(times: number) {
    let [polymer, rules] = getInput();

    let freqs = runPolymerSteps(polymer, times, rules);

    console.log(
        "Score of polymer chain after " + times + " steps: "
        + (Math.max(...Object.values(freqs)) - Math.min(...Object.values(freqs)))
    );
}

function runPolymerSteps(polymer: string, times: number, rules: InsertionRules): PairCount {
    let pairs = getInitialPairMap(polymer, rules);

    for (let i = 0; i < times; i++) {
        pairs = growPolymerOneStep(pairs, rules);
    }

    return Object
        .keys(pairs)
        .reduce(
            (freqs, curr: string) => {
                return Object.assign(
                    freqs,
                    {[curr[0]]: freqs[curr[0]] ? freqs[curr[0]] + pairs[curr] : pairs[curr]}
                )
            },
            {[polymer[polymer.length - 1]]: 1}
        );
}

function getInitialPairMap(polymer: string, rules: InsertionRules): PairCount {
    let pairMap: PairCount = Object
        .keys(rules)
        .reduce((acc, curr) => Object.assign(acc, {[curr]: 0}), {});

    for (let i = 0; i < polymer.length - 1; i++) {
        pairMap[polymer[i] + polymer[i + 1]]++;
    }

    return pairMap;
}

function growPolymerOneStep(pairs: PairCount, rules: InsertionRules): PairCount {
    let newPairs: PairCount = Object.keys(pairs)
        .reduce((obj: PairCount, key) => {
            obj[key] = 0;
            return obj
        }, {});

    for (let pair in newPairs) {
        const currentCount = pairs[pair];
        const insertedChar = rules[pair];

        for (const replacementPair of [pair[0] + insertedChar, insertedChar + pair[1]]) {
            newPairs[replacementPair] += currentCount;
        }
    }

    return newPairs;
}

function getInput(): [string, InsertionRules] {
    let inputParts = fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split("\n\n");

    return [
        inputParts[0],
        inputParts[1]
            .split("\n")
            .map(r => r.split(" -> "))
            .reduce((rules: InsertionRules, x) => {
                rules[x[0]] = x[1];
                return rules;
            }, {})
    ];
}

export {part1, part2};
