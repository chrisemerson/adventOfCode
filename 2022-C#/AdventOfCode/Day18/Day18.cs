using System.Collections.Immutable;

namespace AdventOfCode;

public class Day18 : IAdventOfCodeDay
{
    private static List<(int, int, int)>? containedContiguousAreaCache;
    private static List<(int, int, int)>? nonContainedContiguousAreaCache;

    public Day18()
    {
        containedContiguousAreaCache = new List<(int, int, int)>();
        nonContainedContiguousAreaCache = new List<(int, int, int)>();
    }

    public void Part1(string input) => Console.WriteLine(CountFaces(ParseInput(input)));

    public void Part2(string input)
    {
        var grid = ParseInput(input);
        var faces = CountFaces(grid);

        for (var x = grid.Select(c => c.Item1).Min(); x <= grid.Select(c => c.Item1).Max(); x++) {
            for (var y = grid.Select(c => c.Item2).Min(); y <= grid.Select(c => c.Item2).Max(); y++) {
                for (var z = grid.Select(c => c.Item3).Min(); z <= grid.Select(c => c.Item3).Max(); z++) {
                    if (!grid.Contains((x, y, z)) && !CellCanReachOutside(grid, x, y, z)) {
                        faces -= CountCubesSurroundingCell(grid, (x, y, z));
                    }
                }
            }
        }

        Console.WriteLine(faces);
    }

    private static IEnumerable<(int, int, int)> ParseInput(string input) => input
        .Split("\n", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries)
        .Select(l => l
            .Split(",", StringSplitOptions.TrimEntries)
            .Select(int.Parse))
        .Select(c => (c.ElementAt(0), c.ElementAt(1), c.ElementAt(2)));


    private static int CountFaces(IEnumerable<(int, int, int)> grid) =>
        grid.Aggregate(grid.Count() * 6, (acc, cube) => acc - CountCubesSurroundingCell(grid, cube));

    private static int CountCubesSurroundingCell(IEnumerable<(int, int, int)> grid, (int, int, int) cube) =>
        new List<(int, int, int)> { (-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1) }
            .Select(t => (cube.Item1 + t.Item1, cube.Item2 + t.Item2, cube.Item3 + t.Item3))
            .Count(grid.Contains);

    private static bool CellCanReachOutside(IEnumerable<(int, int, int)> grid, int x, int y, int z)
    {
        if (containedContiguousAreaCache != null && containedContiguousAreaCache.Contains((x, y, z))) {
            return false;
        }

        if (nonContainedContiguousAreaCache != null && nonContainedContiguousAreaCache.Contains((x, y, z))) {
            return true;
        }

        var minX = grid.Select(c => c.Item1).Min();
        var maxX = grid.Select(c => c.Item1).Max();
        var minY = grid.Select(c => c.Item2).Min();
        var maxY = grid.Select(c => c.Item2).Max();
        var minZ = grid.Select(c => c.Item3).Min();
        var maxZ = grid.Select(c => c.Item3).Max();

        var contiguousSpace = new Dictionary<(int, int, int), int> { { (x, y, z), 0 } }.ToImmutableDictionary();

        var oldSpaceSize = 0;

        while (oldSpaceSize != contiguousSpace.Count()) {
            oldSpaceSize = contiguousSpace.Count();

            contiguousSpace = contiguousSpace
                .Where(c => c.Value == contiguousSpace.Select(x => x.Value).Max())
                .Aggregate(contiguousSpace, (acc, c) => new List<(int, int, int)>
                        { (-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1) }
                    .Where(t => !grid.Contains((c.Key.Item1 + t.Item1, c.Key.Item2 + t.Item2, c.Key.Item3 + t.Item3))
                                && !contiguousSpace.ContainsKey(
                                    (c.Key.Item1 + t.Item1, c.Key.Item2 + t.Item2, c.Key.Item3 + t.Item3)))
                    .Aggregate(acc, (acci, nc) => acci
                        .Add((c.Key.Item1 + nc.Item1, c.Key.Item2 + nc.Item2, c.Key.Item3 + nc.Item3), c.Value + 1)));

            if (contiguousSpace.Select(c => c.Key).Any(c =>
                    c.Item1 < minX || c.Item1 > maxX || c.Item2 < minY
                    || c.Item2 > maxY || c.Item3 < minZ || c.Item3 > maxZ
                )) {
                //Our contiguous space has reached outside the bounds of the shape
                foreach (var cell in contiguousSpace) {
                    nonContainedContiguousAreaCache?.Add(cell.Key);
                }

                return true;
            }
        }

        foreach (var cell in contiguousSpace) {
            containedContiguousAreaCache?.Add(cell.Key);
        }

        return false;
    }
}
