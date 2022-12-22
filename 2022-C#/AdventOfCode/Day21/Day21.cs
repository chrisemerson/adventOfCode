namespace AdventOfCode;

public class Day21: IAdventOfCodeDay
{
    private Dictionary<string, string>? monkeys;

    public void Part1(string input)
    {
        ParseInput(input);

        Console.WriteLine(EvaluateMonkey("root"));
    }

    public void Part2(string input)
    {
        ParseInput(input);

        throw new NotImplementedException();
    }

    private void ParseInput(string input)
    {
        monkeys = input
            .Split("\n", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries)
            .Select(l => l.Split(":", StringSplitOptions.TrimEntries))
            .ToDictionary(m => m[0], m => m[1]);
    }

    private long EvaluateMonkey(string monkey)
    {
        if (monkeys == null) return 0;

        if (monkeys[monkey].Contains(" + ")) {
            return monkeys[monkey]
                .Split(" + ")
                .Aggregate((long) 0, (acc, part) => acc + EvaluateMonkey(part));
        }

        if (monkeys[monkey].Contains(" - ")) {
            var parts = monkeys[monkey].Split(" - ");
            return EvaluateMonkey(parts[0]) - EvaluateMonkey(parts[1]);
        }

        if (monkeys[monkey].Contains(" * ")) {
            return monkeys[monkey]
                .Split(" * ")
                .Aggregate((long) 1, (acc, part) => acc * EvaluateMonkey(part));
        }

        if (monkeys[monkey].Contains(" / ")) {
            var parts = monkeys[monkey].Split(" / ");
            return EvaluateMonkey(parts[0]) / EvaluateMonkey(parts[1]);
        }

        return long.Parse(monkeys[monkey]);
    }
}
