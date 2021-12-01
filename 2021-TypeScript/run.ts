(async () => {
    const day = process.argv[2];
    const part = process.argv[3];

    const module = await import("./Day" + day + "/Day" + day);

    switch (part) {
        case "1":
            module.part1();
            break;

        case "2":
            module.part2();
            break;
    }
})();
