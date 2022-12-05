namespace AdventOfCode;

using Stacks = Dictionary<char, Stack<char>>;

public class Day5 : IAdventOfCodeDay
{
    public void Part1(string input) => MoveCrates(input, MoveCrateMultipleTimes);
    public void Part2(string input) => MoveCrates(input, MoveMultipleCratesAtOnce);

    private static void MoveCrates(string input, Func<Stacks, char, char, int, Stacks> crateMovingStrategy)
    {
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

    private static Stacks CreateStacks(string stacksString) => Enumerable
        .Range(0, stacksString.Split("\n").Last().Length)
        .Where(i => stacksString.Split("\n").Last()[i] != ' ')
        .ToDictionary(
            i => stacksString.Split("\n").Last()[i],
            i => new Stack<char>(stacksString
                .Split("\n")
                .Reverse()
                .Skip(1)
                .Select(x => x.PadRight(i + 1)[i])
                .Where(x => x != ' ')));

    private static Stacks ProcessInstruction(
        Stacks state,
        string instruction,
        Func<Stacks, char, char, int, Stacks> crateMovingStrategy
    ) {
        var instructionParts = instruction.Split(" ");

        return crateMovingStrategy(
            state,
            instructionParts[3][0],
            instructionParts[5][0],
            int.Parse(instructionParts[1])
        );
    }

    private static Stacks MoveCrateMultipleTimes(Stacks state, char stackFrom, char stackTo, int times)
    {
        for (var i = 0; i < times; i++) {
            state[stackTo].Push(state[stackFrom].Pop());
        }

        return state;
    }

    private static Stacks MoveMultipleCratesAtOnce(Stacks state, char stackFrom, char stackTo, int crates)
    {
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
