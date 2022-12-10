using System.Collections.Immutable;

namespace AdventOfCode;

public class Day10 : IAdventOfCodeDay
{
    private const string InstructionNoop = "noop";
    private const string InstructionAddx = "addx";

    public void Part1(string input) => Console.WriteLine(
        RunProgram(input)
            .Aggregate(0, (acc, x) => (x.Key - 20) % 40 == 0
                ? acc + x.Key * x.Value
                : acc));

    public void Part2(string input) => RunProgram(input)
        .Select(x => new KeyValuePair<int, bool>(x.Key, Math.Abs((x.Key - 1) % 40 - x.Value) <= 1))
        .Chunk(40)
        .ToList()
        .ForEach(l => Console.WriteLine(string.Concat(l.Select(p => p.Value ? '█' : ' '))));

    private static ImmutableDictionary<int, int> RunProgram(string input) => input
        .Split("\n", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
        .Select(l => l.Split(" "))
        .Aggregate(new Dictionary<int, int> { { 0, 1 }, { 1, 1 } }.ToImmutableDictionary(), ProcessInstruction);

    private static ImmutableDictionary<int, int> ProcessInstruction(
        ImmutableDictionary<int, int> state,
        string[] instruction
    ) => instruction[0] switch {
        InstructionNoop => state.Add(state.Count, state.Last().Value),
        InstructionAddx => state
            .Add(state.Count, state.Last().Value)
            .Add(state.Count + 1, state.Last().Value + int.Parse(instruction[1])),
        _ => state
    };
}
