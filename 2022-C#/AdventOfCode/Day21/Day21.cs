namespace AdventOfCode;

public class Day21 : IAdventOfCodeDay
{
    private Dictionary<string, string>? monkeys;
    private Dictionary<string, List<string>>? dependencies;

    public void Part1(string input)
    {
        ParseInput(input);

        Console.WriteLine(EvaluateMonkey("root"));
    }

    public void Part2(string input)
    {
        ParseInput(input);

        if (monkeys == null || dependencies == null) {
            throw new Exception();
        }

        var parts = monkeys["root"].Split(" ");
        long requiredValue;

        if (dependencies[parts[0]].Contains("humn")) {
            requiredValue = DetermineMonkeyValue(parts[0], EvaluateMonkey(parts[2]));
        } else {
            requiredValue = DetermineMonkeyValue(parts[2], EvaluateMonkey(parts[0]));
        }

        Console.WriteLine(requiredValue);
    }

    private void ParseInput(string input)
    {
        monkeys = input
            .Split("\n", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries)
            .Select(l => l.Split(":", StringSplitOptions.TrimEntries))
            .ToDictionary(m => m[0], m => m[1]);

        dependencies = monkeys.ToDictionary(m => m.Key, m => GetDependencies(m.Key));
    }

    private List<string> GetDependencies(string monkey)
    {
        if (monkeys == null || !monkeys[monkey].Contains(" ")) {
            return new List<string>();
        }

        var parts = monkeys[monkey].Split(" ", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries);
        var deps = new List<string> { parts[0], parts[2] };

        deps.AddRange(GetDependencies(parts[0]));
        deps.AddRange(GetDependencies(parts[2]));

        return deps;
    }

    private long EvaluateMonkey(string monkey)
    {
        if (monkeys == null) return 0;

        if (monkeys[monkey].Contains(" + ")) {
            return monkeys[monkey]
                .Split(" + ")
                .Aggregate((long)0, (acc, part) => acc + EvaluateMonkey(part));
        }

        if (monkeys[monkey].Contains(" - ")) {
            var parts = monkeys[monkey].Split(" - ");
            return EvaluateMonkey(parts[0]) - EvaluateMonkey(parts[1]);
        }

        if (monkeys[monkey].Contains(" * ")) {
            return monkeys[monkey]
                .Split(" * ")
                .Aggregate((long)1, (acc, part) => acc * EvaluateMonkey(part));
        }

        if (monkeys[monkey].Contains(" / ")) {
            var parts = monkeys[monkey].Split(" / ");
            return EvaluateMonkey(parts[0]) / EvaluateMonkey(parts[1]);
        }

        return long.Parse(monkeys[monkey]);
    }

    private long DetermineMonkeyValue(string monkey, long requiredValue)
    {
        if (monkey == "humn") {
            return requiredValue;
        }

        if (monkeys == null || dependencies == null) {
            return 0;
        }

        var parts = monkeys[monkey].Split(" ", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries);

        if (dependencies[parts[0]].Contains("humn") || parts[0] == "humn") {
            return parts[1] switch {
                "+" => DetermineMonkeyValue(parts[0], requiredValue - EvaluateMonkey(parts[2])),
                "-" => DetermineMonkeyValue(parts[0], requiredValue + EvaluateMonkey(parts[2])),
                "*" => DetermineMonkeyValue(parts[0], requiredValue / EvaluateMonkey(parts[2])),
                "/" => DetermineMonkeyValue(parts[0], requiredValue * EvaluateMonkey(parts[2])),
                _ => DetermineMonkeyValue(parts[0], 0)
            };
        } else {
            return parts[1] switch {
                "+" => DetermineMonkeyValue(parts[2], requiredValue - EvaluateMonkey(parts[0])),
                "-" => DetermineMonkeyValue(parts[2], EvaluateMonkey(parts[0]) - requiredValue),
                "*" => DetermineMonkeyValue(parts[2], requiredValue / EvaluateMonkey(parts[0])),
                "/" => DetermineMonkeyValue(parts[2], EvaluateMonkey(parts[0]) / requiredValue),
                _ => DetermineMonkeyValue(parts[2], 0)
            };
        }
    }
}
