using System.Collections.Immutable;

namespace AdventOfCode;

public class Day24 : IAdventOfCodeDay
{
    public void Part1(string input)
    {
        var grid = ParseInput(input);

        var start = grid.First(gs => gs is { Value: '.', Key.Item1: 0 }).Key;
        var goal = grid.First(gs => gs.Value == '.' && gs.Key.Item1 == grid.Max(s => s.Key.Item1)).Key;

        Console.WriteLine("Goal reached in " + CalculateLengthOfRoute(
            new List<(int, int)> { start, goal },
            grid
                .Where(gs => gs.Value != '.' && gs.Value != '#')
                .Select((gs, i) => (i, gs.Key, gs.Value))
                .ToDictionary(i => (i.Item1, i.Item3), i => i.Item2),
            grid
                .Select(ch => (ch.Key, ch.Value == '#' ? '#' : '.'))
                .ToImmutableDictionary(gs => gs.Item1, gs => gs.Item2)
        ) + " minutes");
    }

    public void Part2(string input)
    {
        var grid = ParseInput(input);

        var start = grid.First(gs => gs is { Value: '.', Key.Item1: 0 }).Key;
        var goal = grid.First(gs => gs.Value == '.' && gs.Key.Item1 == grid.Max(s => s.Key.Item1)).Key;

        Console.WriteLine("Goal reached in " + CalculateLengthOfRoute(
            new List<(int, int)> { start, goal, start, goal },
            grid
                .Where(gs => gs.Value != '.' && gs.Value != '#')
                .Select((gs, i) => (i, gs.Key, gs.Value))
                .ToDictionary(i => (i.Item1, i.Item3), i => i.Item2),
            grid
                .Select(ch => (ch.Key, ch.Value == '#' ? '#' : '.'))
                .ToImmutableDictionary(gs => gs.Item1, gs => gs.Item2)
        ) + " minutes");
    }

    private static int CalculateLengthOfRoute(
        IReadOnlyList<(int, int)> route,
        Dictionary<(int, char), (int, int)> blizzards,
        ImmutableDictionary<(int, int), char> grid
    ) {
        var minute = 0;

        for (var i = 0; i < route.Count - 1; i++) {
            var currentPosition = route[i];
            var goal = route[i + 1];

            var possiblePositions = new List<(int, int)> { currentPosition };

            while (!possiblePositions.Contains(goal)) {
                blizzards = MigrateBlizzards(blizzards, grid);

                possiblePositions = possiblePositions
                    .Select(p => new List<(int, int)> { (0, 0), (0, 1), (0, -1), (1, 0), (-1, 0) }
                        .Select(v => (p.Item1 + v.Item1, p.Item2 + v.Item2))
                        .Where(pp => grid.ContainsKey(pp) && grid[pp] == '.')
                        .Where(pp => !blizzards.ContainsValue(pp)))
                    .Aggregate(new List<(int, int)>(), (acc, c) => acc.Concat(c).ToList())
                    .Distinct()
                    .ToList();

                Console.WriteLine("At minute " + minute + " there are " + possiblePositions.Count + " possible positions");
                minute++;
            }
        }

        return minute;
    }

    private static Dictionary<(int, char), (int, int)> MigrateBlizzards(
        Dictionary<(int, char), (int, int)> blizzards,
        ImmutableDictionary<(int, int), char> grid
    ) => blizzards
        .Select(b => KeyValuePair.Create(b.Key, MigrateBlizzard(b.Value, b.Key.Item2, grid)))
        .ToDictionary(b => b.Key, b => b.Value);

    private static (int, int) MigrateBlizzard(
        (int, int) blizzardLocation,
        char blizzardDirection,
        ImmutableDictionary<(int, int), char> grid
    ) {
        var directionChangeVector = blizzardDirection switch {
            '>' => (0, 1),
            'v' => (1, 0),
            '<' => (0, -1),
            '^' => (-1, 0),
            _ => (0, 0)
        };

        var gridHeight = grid.Select(gc => gc.Key.Item1).Max();
        var gridWidth = grid.Select(gc => gc.Key.Item2).Max();

        do {
            blizzardLocation = (
                (blizzardLocation.Item1 + directionChangeVector.Item1 + gridHeight) % gridHeight,
                (blizzardLocation.Item2 + directionChangeVector.Item2 + gridWidth) % gridWidth
            );
        } while (grid[blizzardLocation] == '#');

        return blizzardLocation;
    }

    private static ImmutableDictionary<(int, int), char> ParseInput(string input) => input
        .Split("\n", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries)
        .Select((l, i) => l
            .Select((c, ii) => new { ii, c })
            .ToDictionary(e => (i, e.ii), e => e.c))
        .Aggregate(
            new Dictionary<(int, int), char>().ToImmutableDictionary(),
            (acc, c) => c.Aggregate(acc, (accInner, ci) => accInner.Add(ci.Key, ci.Value)));
}
