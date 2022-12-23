using System.Collections.Immutable;

namespace AdventOfCode;

public class Day23 : IAdventOfCodeDay
{
    public void Part1(string input)
    {
        var elves = ParseInput(input);

        const int rounds = 10;
        var directionToTryFirst = 0;

        for (var i = 0; i < rounds; i++) {
            elves = GetElfProposals(elves, directionToTryFirst).Aggregate(elves, (acc, elf) => acc.Remove(elf.Key).Add(elf.Value));
            directionToTryFirst = (directionToTryFirst + 1) % 4;
        }

        var areaCovered = (elves.Select(e => e.Item2).Max() - elves.Select(e => e.Item2).Min() + 1) *
                          (elves.Select(e => e.Item1).Max() - elves.Select(e => e.Item1).Min() + 1)
                          - elves.Count;

        Console.WriteLine("Empty ground tiles in region: " + areaCovered);
    }

    public void Part2(string input)
    {
        var elves = ParseInput(input);

        var round = 0;
        var directionToTryFirst = 0;
        var elvesMoving = true;

        while (elvesMoving) {
            var elfProposals = GetElfProposals(elves, directionToTryFirst);
            elves = elfProposals.Aggregate(elves, (acc, elf) => acc.Remove(elf.Key).Add(elf.Value));
            directionToTryFirst = (directionToTryFirst + 1) % 4;

            elvesMoving = elfProposals.Any(e => e.Key != e.Value);
            round++;
        }

        Console.WriteLine("First round with no elf movement: " + round);
    }

    private static ImmutableList<(int, int)> ParseInput(string input) => input
        .Split("\n", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries)
        .Select((l, i) => new { i, l = l.Select((c, ii) => new { ii, c }).Where(c => c.c == '#') })
        .Aggregate(
            new List<(int, int)>().ToImmutableList(),
            (acc, l) => l.l.Aggregate(acc, (acci, c) => acci.Add((l.i, c.ii))));

    private static Dictionary<(int, int), (int, int)> GetElfProposals(
        IEnumerable<(int, int)> elves,
        int directionToTryFirst = 0
    ) {
        var elfProposals = new Dictionary<(int, int), (int, int)>();

        foreach (var elf in elves) {

            if (elves.Contains((elf.Item1 - 1, elf.Item2 - 1))
                || elves.Contains((elf.Item1 - 1, elf.Item2))
                || elves.Contains((elf.Item1 - 1, elf.Item2 + 1))
                || elves.Contains((elf.Item1, elf.Item2 - 1))
                || elves.Contains((elf.Item1, elf.Item2 + 1))
                || elves.Contains((elf.Item1 + 1, elf.Item2 - 1))
                || elves.Contains((elf.Item1 + 1, elf.Item2))
                || elves.Contains((elf.Item1 + 1, elf.Item2 + 1))) {

                foreach (var direction in Enumerable.Range(0, 4)) {
                    if ((direction + directionToTryFirst) % 4 == 0
                        && !elfProposals.ContainsKey(elf)
                        && !elves.Contains((elf.Item1 - 1, elf.Item2 - 1))
                        && !elves.Contains((elf.Item1 - 1, elf.Item2))
                        && !elves.Contains((elf.Item1 - 1, elf.Item2 + 1))) {
                        elfProposals.Add(elf, (elf.Item1 - 1, elf.Item2));
                    }

                    if ((direction + directionToTryFirst) % 4 == 1
                        && !elfProposals.ContainsKey(elf)
                        && !elves.Contains((elf.Item1 + 1, elf.Item2 - 1))
                        && !elves.Contains((elf.Item1 + 1, elf.Item2))
                        && !elves.Contains((elf.Item1 + 1, elf.Item2 + 1))) {
                        elfProposals.Add(elf, (elf.Item1 + 1, elf.Item2));
                    }

                    if ((direction + directionToTryFirst) % 4 == 2
                        && !elfProposals.ContainsKey(elf)
                        && !elves.Contains((elf.Item1 - 1, elf.Item2 - 1))
                        && !elves.Contains((elf.Item1, elf.Item2 - 1))
                        && !elves.Contains((elf.Item1 + 1, elf.Item2 - 1))) {
                        elfProposals.Add(elf, (elf.Item1, elf.Item2 - 1));
                    }

                    if ((direction + directionToTryFirst) % 4 == 3
                        && !elfProposals.ContainsKey(elf)
                        && !elves.Contains((elf.Item1 - 1, elf.Item2 + 1))
                        && !elves.Contains((elf.Item1, elf.Item2 + 1))
                        && !elves.Contains((elf.Item1 + 1, elf.Item2 + 1))) {
                        elfProposals.Add(elf, (elf.Item1, elf.Item2 + 1));
                    }
                }
            }

            if (!elfProposals.ContainsKey(elf)) {
                elfProposals.Add(elf, elf);
            }
        }

        return FilterDuplicates(elfProposals);
    }

    private static Dictionary<(int, int), (int, int)> FilterDuplicates(Dictionary<(int, int), (int, int)> dict) => dict
        .Where(entry => dict.Count(e => e.Value == entry.Value) == 1)
        .ToDictionary(entry => entry.Key, entry => entry.Value);
}
