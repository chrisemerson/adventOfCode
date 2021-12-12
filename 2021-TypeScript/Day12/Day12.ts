import * as fs from 'fs';

type Map = {[key: string]: string[]};

type CurrentState = {
    visited: string[],
    currentPath: string[]
};

function part1() {
    const paths = findPaths(
        {
            visited: ['start'],
            currentPath: ['start']
        },
        getInput(),
        canVisitSmallCavesAtMostOnce
    );

    console.log("There are " + paths.length + " paths through this cave system");
}

function part2() {
    const paths = findPaths(
        {
            visited: ['start'],
            currentPath: ['start']
        },
        getInput(),
        canVisitSingleSmallCaveTwice
    );

    console.log("There are " + paths.length + " paths through this cave system");
}

function findPaths(currentState: CurrentState, caveMap: Map, availableCavesCallback: (a: string[], b: string) => boolean): string[][] {
    let paths = [];

    const currentCave = currentState.currentPath[currentState.currentPath.length - 1];

    if (currentCave === 'end') {
        return [currentState.currentPath];
    }

    const pathPossibilities = caveMap[currentCave]
        .filter(c => isBigCave(c) || availableCavesCallback(currentState.visited, c));

    for (const pathPossibility of pathPossibilities) {
        paths.push(
            ...findPaths(
                {
                    visited: currentState.visited.concat(pathPossibility),
                    currentPath: currentState.currentPath.concat(pathPossibility)
                },
                caveMap,
                availableCavesCallback
            )
        );
    }

    return paths;
}

function canVisitSmallCavesAtMostOnce(visited: string[], cave: string): boolean {
    return !visited.includes(cave);
}

function canVisitSingleSmallCaveTwice(visited: string[], cave: string): boolean {
    if (cave === 'start') {
        return false;
    }

    //Remove big caves, we don't need to consider them
    visited = visited.filter(c => !isBigCave(c));

    //Find out how many times we've visited each cave
    const frequencies = visited.reduce((f:{[_: string]: number}, c) => {
        if (!f[c]) f[c] = 0;
        f[c]++;
        return f;
    }, {});

    //If we haven't visited any cave twice yet, we can always visit
    if (Object.values(frequencies).filter(f => f === 2).length === 0) {
        return true;
    }

    return !visited.includes(cave);
}

function isBigCave(cave: string): boolean {
    return cave === cave.toUpperCase();
}

function getInput(): Map {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split("\n")
        .map(l => l.split('-'))
        .reduce((map: Map, x: string[]) => {
            if (!map[x[0]]) map[x[0]] = [];
            if (!map[x[1]]) map[x[1]] = [];

            map[x[0]].push(x[1]);
            map[x[1]].push(x[0]);

            return map;
        }, {});
}

export {part1, part2};
