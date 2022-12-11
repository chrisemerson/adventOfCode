namespace AdventOfCode;

public class Day11 : IAdventOfCodeDay
{
    public void Part1(string input)
    {
        var monkeys = ParseMonkeys(input);

        for (var i = 1; i <= 20; i++) {
            foreach (var m in monkeys.Select(kv => kv.Key)) {
                while (monkeys[m].HasItems()) {
                    var (nextMonkey, item) = monkeys[m].InspectItem();
                    monkeys[nextMonkey].AcceptItem(item);
                }
            }
        }

        Console.WriteLine(monkeys
            .OrderByDescending(m => m.Value.GetInspectionCount())
            .Take(2)
            .Aggregate((ulong)1, (acc, m) => acc * m.Value.GetInspectionCount()));
    }

    public void Part2(string input)
    {
        var monkeys = ParseMonkeys(input);
        var moduloToUse = monkeys.Select(m => m.Value.DivisorForTest).Aggregate((ulong) 1, (acc, d) => acc * d);

        for (var i = 1; i <= 10000; i++) {
            foreach (var m in monkeys.Select(kv => kv.Key)) {
                while (monkeys[m].HasItems()) {
                    var (nextMonkey, item) = monkeys[m].InspectItemHarder(moduloToUse);
                    monkeys[nextMonkey].AcceptItem(item);
                }
            }
        }

        Console.WriteLine(monkeys
            .OrderByDescending(m => m.Value.GetInspectionCount())
            .Take(2)
            .Aggregate((ulong)1, (acc, m) => acc * m.Value.GetInspectionCount()));
    }

    private static IDictionary<int, Monkey> ParseMonkeys(string input) => input
        .Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
        .Select(m => ParseMonkey(m.Split("\n", StringSplitOptions.RemoveEmptyEntries).ToArray()))
        .ToDictionary(m => m.Id, m => m);

    private static Monkey ParseMonkey(string[] monkeyLines)
    {
        var (op, operand1, operand2) = ParseOperation(
            monkeyLines.First(l => l.StartsWith("  Operation: new = "))[19..]
        );

        return new Monkey(
            int.Parse(monkeyLines.First(l => l.StartsWith("Monkey ")).Substring(7, 1)),
            op,
            operand1,
            operand2,
            ulong.Parse(monkeyLines.First(l => l.StartsWith("  Test: divisible by "))[21..]),
            int.Parse(monkeyLines.First(l => l.StartsWith("    If true: throw to monkey "))[29..]),
            int.Parse(monkeyLines.First(l => l.StartsWith("    If false: throw to monkey "))[30..]),
            monkeyLines.First(l => l.StartsWith("  Starting items: "))[18..].Split(",").Select(ulong.Parse).ToList()
        );
    }

    private static (Operator, ulong?, ulong?) ParseOperation(string operationString)
    {
        var equation = operationString.Split(" ", StringSplitOptions.TrimEntries);

        return (
            equation[1] == "*" ? Operator.Multiply : Operator.Add,
            equation[0] == "old" ? null : ulong.Parse(equation[0]),
            equation[2] == "old" ? null : ulong.Parse(equation[2])
        );
    }
}
