import * as fs from 'fs';

interface WiringConfiguration {
    digits: string[];
    output: string[];
}

enum Segment {
    Top,
    TopLeft,
    TopRight,
    Middle,
    BottomLeft,
    BottomRight,
    Bottom
}

type DecodedSignal = {[_ in keyof typeof Segment]: string};

function part1 () {
    const digitCount = getInput()
        .map(w => w.output)
        .map(o => o.filter(d => d.length === 2 || d.length === 4 || d.length === 3 || d.length === 7))
        .map(ud => ud.length)
        .reduce((acc, x) => acc + x, 0);

    console.log("Number of unique digit lengths in output numbers: " + digitCount);
}

function part2 () {
    let input = getInput();

    const outputSum = input
        .map(i => {
            const outputDigits = i.output.map(od => getDigit(od, decodeSignal(i)));
            return outputDigits[0] * 1000 + outputDigits[1] * 100 + outputDigits[2] * 10 + outputDigits[3];
        })
        .reduce((acc, x) => acc + x, 0);

    console.log("Sum of outputs is " + outputSum);
}

function decodeSignal(wc: WiringConfiguration): DecodedSignal {
    //First, find the 1 and its segments
    const oneSegments = wc.digits
        .filter(d => d.length === 2)[0]
        .split('');

    //Find 7 - the segment that isn't contained in 1 is the Top
    const top = wc.digits
        .filter(d => d.length === 3)[0]
        .split('')
        .filter(l => !oneSegments.includes(l))[0];

    //Find 3 - the only digit with length 5 and both items in common with 1
    const threeSegments = wc.digits
        .filter(d => d.length === 5)
        .filter(d => d
            .split('')
            .filter(v => oneSegments.includes(v))
            .length === oneSegments.length
        )[0]
        .split('');

    //Find 4 - the only digit with length 4
    const fourSegments = wc.digits
        .filter(d => d.length === 4)[0]
        .split('');

    //Find the common segment between 3 and 4 that isn't in 1 - this is the middle segment
    const middle = threeSegments
        .filter(l => !oneSegments.includes(l))
        .filter(l => fourSegments.filter(ll => !oneSegments.includes(ll)).includes(l))[0];

    //Find the segment of 3 that isn't in 1, isn't the middle or top - this is the bottom
    const bottom = threeSegments
        .filter(l => !oneSegments.includes(l) && l !== top && l != middle)[0];

    //Find the 9 - it has all the digits that are in 3 but is length 6. The extra segment is top left
    const topLeft = wc.digits
        .filter(d => d.length === 6)
        .filter(d => d
            .split('')
            .filter(ll => threeSegments.includes(ll))
            .length === threeSegments.length
        )[0]
        .split('')
        .filter(l => !threeSegments.includes(l))[0];

    //Find the 5 - it has length 5 and includes top left. Its segment in common with 1 is bottom right.
    const bottomRight = wc.digits
        .filter(d => d.length === 5)
        .filter(d => d.split('').includes(topLeft))[0]
        .split('')
        .filter(l => oneSegments.includes(l))[0];

    //Top right must be the other segment from 1
    const topRight = oneSegments.filter(l => l !== bottomRight)[0];

    //And finally, bottom left is the one that isn't the others - use digit 8 to find it!
    const bottomLeft = wc.digits
        .filter(d => d.length === 7)[0]
        .split('')
        .filter(l => ![top, topRight, topLeft, middle, bottomRight, bottom].includes(l))[0];

    return {
        Top: top,
        TopLeft: topLeft,
        TopRight: topRight,
        Middle: middle,
        BottomLeft: bottomLeft,
        BottomRight: bottomRight,
        Bottom: bottom
    };
}

function getDigit(digitString: string, decodedSignal: DecodedSignal): number {
    let segmentList: Segment[];

    switch (digitString.length) {
        case 2:
            return 1;

        case 3:
            return 7;

        case 4:
            return 4;

        case 7:
            return 8;

        case 5:
            segmentList = convertDigitsToSegments(digitString, decodedSignal);

            if (segmentList.includes(Segment.TopLeft)) {
                return 5;
            } else if (segmentList.includes(Segment.BottomLeft)) {
                return 2;
            } else {
                return 3;
            }

        case 6:
            segmentList = convertDigitsToSegments(digitString, decodedSignal);

            if (!segmentList.includes(Segment.Middle)) {
                return 0;
            } else if (segmentList.includes(Segment.BottomLeft)) {
                return 6;
            } else {
                return 9;
            }
    }

    return 0;
}

function convertDigitsToSegments(digitString: string, decodedSignal: DecodedSignal): Segment[] {
    return digitString
        .split('')
        .map(l => Object.keys(decodedSignal)
            // @ts-ignore
            .filter(s => decodedSignal[s] === l)[0]
        // @ts-ignore
        ).map(key => Segment[key]);
}

function getInput(): WiringConfiguration[] {
    return fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split("\n")
        .map(l => {
            const wiring = l.split('|');

            return {
                digits: wiring[0].trim().split(' '),
                output: wiring[1].trim().split(' ')
            }
        });
}

export { part1, part2 };
