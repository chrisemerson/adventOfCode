namespace AdventOfCode;

public class Day3 : IAdventOfCodeDay
{
    public void Part1(string input) => Console.WriteLine(input
        .Split("\n")
        .Where(l => l != "")
        .Select(s => s[..(s.Length / 2)]
            .ToHashSet()
            .Intersect(s[(s.Length / 2)..])
            .ElementAt(0))
        .Select(Priority)
        .Sum());

    public void Part2(string input) => Console.WriteLine(input
        .Split("\n")
        .Where(l => l != "")
        .Chunk(3)
        .Select(g => g[0].ToHashSet()
            .Intersect(g[1])
            .Intersect(g[2])
            .ElementAt(0))
        .Select(Priority)
        .Sum());

    private static int Priority(char c) => c > 96 ? c - 96 : c - 38;
}
