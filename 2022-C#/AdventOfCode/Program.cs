using AdventOfCode;

if (args.Length >= 2) {
    var dayString = args[0];
    var partString = args[1];

    var day = Type.GetType("AdventOfCode.Day" + dayString);

    if (day != null && Activator.CreateInstance(day) is IAdventOfCodeDay dayInstance) {
        var input = "";

        if (args.Length == 3) {
            input = args[2];
        } else {
            var inputFile = Directory.GetCurrentDirectory() + "/Day" + dayString + "/input.txt";

            if (File.Exists(inputFile)) {
                input = File.ReadAllText(inputFile);
            } else {
                Console.Error.WriteLine("Input not provided and input file not found");
                Environment.Exit(1);
            }
        }

        switch (partString) {
            case "1":
                dayInstance.Part1(input);
                break;

            case "2":
                dayInstance.Part2(input);
                break;

            default:
                Console.Error.WriteLine($"Invalid Part no: {partString}");
                break;
        }
    } else {
        Console.Error.WriteLine($"Invalid Day no: {dayString}");
    }
} else {
    Console.Error.WriteLine("Day and Part number required");
}