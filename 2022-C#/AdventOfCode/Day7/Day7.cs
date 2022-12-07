using System.Collections.Immutable;

namespace AdventOfCode;

public class Day7 : IAdventOfCodeDay
{
    private const int TotalSize = 70000000;
    private const int SpaceRequired = 30000000;

    public void Part1(string input) => Console.WriteLine(
        GetFileSystem(input)
            .Where(d => d.Value <= 100000)
            .Select(d => d.Value)
            .Sum());

    public void Part2(string input) => Console.WriteLine(
        GetFileSystem(input)
            .Where(d => d.Value >= SpaceRequired + GetFileSystem(input)["/"] - TotalSize)
            .Min(d => d.Value));

    private static ImmutableDictionary<string, int> GetFileSystem(string input) => input
        .Split("\n")
        .Where(l => l != "")
        .Aggregate(
            ("/", new Dictionary<string, int> { { "/", 0 } }.ToImmutableDictionary()),
            (fs, l) => ProcessInputLine(fs, l.Split(" ")))
        .Item2;

    private static (string, ImmutableDictionary<string, int>) ProcessInputLine(
        (string, ImmutableDictionary<string, int>) fs,
        IReadOnlyList<string> inputLineParts
    ) => inputLineParts.ElementAt(0) switch {
        "$" => ProcessInstruction(fs, inputLineParts.Skip(1)),
        "dir" => fs,
        _ => ProcessFileEntry(fs, int.Parse(inputLineParts.ElementAt(0)))
    };

    private static (string, ImmutableDictionary<string, int>) ProcessInstruction(
        (string, ImmutableDictionary<string, int>) fs,
        IEnumerable<string> instruction
    ) => instruction.ElementAt(0) switch {
        "cd" => ProcessDirectoryChange(fs, instruction.ElementAt(1)),
        _ => fs
    };

    private static (string, ImmutableDictionary<string, int>) ProcessDirectoryChange(
        (string, ImmutableDictionary<string, int>) fs,
        string directory
    ) => directory switch {
        "/" => ("/", fs.Item2),
        ".." => (string.Join('/', fs.Item1.Split("/").SkipLast(1)), fs.Item2),
        _ => (
            fs.Item1 + directory + "/",
            fs.Item2.Remove(fs.Item1 + directory + "/").Add(fs.Item1 + directory + "/", 0))
    };

    private static (string, ImmutableDictionary<string, int>) ProcessFileEntry(
        (string, ImmutableDictionary<string, int>) fs,
        int fileSize
    ) => fs.Item1
        .Split("/")
        .SkipLast(1)
        .Aggregate(("", fs.Item2), (f, dir) => (
            f.Item1 + dir + "/",
            f.Item2
                .Remove(f.Item1 + dir + "/")
                .Add(f.Item1 + dir + "/", fs.Item2[f.Item1 + dir + "/"] + fileSize)));
}
