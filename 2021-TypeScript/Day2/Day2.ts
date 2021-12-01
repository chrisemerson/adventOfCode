import * as fs from 'fs';

function part1 () {

}

function part2 () {

}

function getInput() {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .split("\n")
        .map(l => parseInt(l));
}

export { part1, part2 };
