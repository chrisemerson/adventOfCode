import * as fs from 'fs';

function part1() {
    const [algorithm, image] = getInput();

    outputLitPixelsAfterEnhancement(image, algorithm, 2);
}

function part2() {
    const [algorithm, image] = getInput();

    outputLitPixelsAfterEnhancement(image, algorithm, 50);
}

function outputLitPixelsAfterEnhancement(image: string[][], algorithm: string, times: number): void {
    let enhancedImage = enhanceImageNoTimes(image, algorithm, times);

    const litPixels = enhancedImage
        .map(line => line.reduce((acc, c) => acc + parseInt(c), 0))
        .reduce((acc, c) => acc + c, 0);

    console.log("There are " + litPixels + " let pixels in the image after enhancing " + times + " times");
}

function enhanceImageNoTimes(image: string[][], algorithm: string, times: number): string[][] {
    let enhancedImage = image;

    // On the first run, we fill in the infinite void with 0
    let expandedImage = expandImage(image, '0');

    for (let i = 0; i < times; i++) {
        enhancedImage = enhanceImage(expandedImage, algorithm);
        expandedImage = expandImage(enhancedImage, enhancedImage[0][0]);
    }

    return enhancedImage;
}

function expandImage(image: string[][], charToExpandWith = '0'): string[][] {
    let outputImage: string[][] = [];

    outputImage.push(charToExpandWith.repeat(image[0].length + 4).split(''));
    outputImage.push(charToExpandWith.repeat(image[0].length + 4).split(''));

    for (let i = 0; i < image.length; i++) {
        let line = [charToExpandWith, charToExpandWith];

        for (let j = 0; j < image.length; j++) {
            line.push(image[i][j]);
        }

        line.push(charToExpandWith);
        line.push(charToExpandWith);

        outputImage.push(line);
    }

    outputImage.push(charToExpandWith.repeat(image[0].length + 4).split(''));
    outputImage.push(charToExpandWith.repeat(image[0].length + 4).split(''));

    return outputImage;
}

function enhanceImage(image: string[][], algorithm: string): string[][] {
    let outputImage: string[][] = [];

    for (let i = 0; i < image.length; i++) {
        outputImage[i] = [];

        for (let j = 0; j < image[i].length; j++) {
            let binaryString;

            if (i === 0 || j === 0 || i === (image.length - 1) || j === (image[i].length - 1)) {
                binaryString = image[i][j].repeat(9);
            } else {
                binaryString =
                    image[i - 1][j - 1] + image[i - 1][j] + image[i - 1][j + 1]
                    + image[i][j - 1] + image[i][j] + image[i][j + 1]
                    + image[i + 1][j - 1] + image[i + 1][j] + image[i + 1][j + 1]
            }

            outputImage[i][j] = algorithm[parseInt(binaryString, 2)];
        }
    }

    return outputImage;
}

function getInput(): [string, string[][]] {
    const input = fs
        .readFileSync(__dirname + "/input.txt", 'utf8')
        .trim()
        .split('\n\n');

    const image = input[1]
        .split("\n")
        .map(line => line
            .split('')
            .map(char => char == '#' ? '1' : '0'));

    return [input[0].split('#').join('1').split('.').join('0'), image];
}

export {part1, part2};
