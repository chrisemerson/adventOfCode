namespace AdventOfCode;

public class Day5 : IAdventOfCodeDay
{
    public void Part1(string input) => MoveCrates(input, MoveCrateMultipleTimes);
    public void Part2(string input) => MoveCrates(input, MoveMultipleCratesAtOnce);

    private static void MoveCrates(
        string input,
        Func<Dictionary<int, Stack<char>>, int, int, int, Dictionary<int, Stack<char>>> crateMovingStrategy
    ) {
        var inputParts = input.Split("\n\n");

        Console.WriteLine(
            string.Join("", inputParts[1]
                .Split("\n")
                .Where(i => i != "")
                .Aggregate(
                    CreateStacks(inputParts[0]),
                    (agg, x) => ProcessInstruction(agg, x, crateMovingStrategy)
                ).Select(s => s.Value.Peek())));
    }

    private static Dictionary<int, Stack<char>> CreateStacks(string stacksString) => Enumerable
        .Range(1, 9)
        .ToDictionary(
            i => i,
            i => new Stack<char>(stacksString
                .Split("\n")
                .Reverse()
                .Skip(1)
                .Select(x => x[4 * i - 3])
                .Where(x => x != ' ')));

    private static Dictionary<int, Stack<char>> ProcessInstruction(
        Dictionary<int, Stack<char>> state,
        string instruction,
        Func<Dictionary<int, Stack<char>>, int, int, int, Dictionary<int, Stack<char>>> crateMovingStrategy
    ) {
        var instructionParts = instruction.Split(" ");

        return crateMovingStrategy(
            state,
            int.Parse(instructionParts[3]),
            int.Parse(instructionParts[5]),
            int.Parse(instructionParts[1])
        );
    }

    private static Dictionary<int, Stack<char>> MoveCrateMultipleTimes(
        Dictionary<int, Stack<char>> state,
        int stackFrom,
        int stackTo,
        int times
    ) {
        for (var i = 0; i < times; i++)
        {
            state[stackTo].Push(state[stackFrom].Pop());
        }

        return state;
    }

    private static Dictionary<int, Stack<char>> MoveMultipleCratesAtOnce(
        Dictionary<int, Stack<char>> state,
        int stackFrom,
        int stackTo,
        int crates
    ) {
        var removedCrates = new List<char>();

        for (var i = 0; i < crates; i++) {
            removedCrates.Add(state[stackFrom].Pop());
        }

        removedCrates.Reverse();

        foreach (var c in removedCrates) {
            state[stackTo].Push(c);
        }

        return state;
    }
}
