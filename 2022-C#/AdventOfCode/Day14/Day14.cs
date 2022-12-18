using System.Collections.Immutable;

namespace AdventOfCode;

public class Day14 : IAdventOfCodeDay
{
    public void Part1(string input)
    {
        var grid = CreateGridFromInput(input);

        var addingSand = true;
        var sandAdded = 0;

        while (addingSand) {
            var result = AddSand(grid);

            if (result == null) {
                addingSand = false;
            } else {
                grid = result;
                sandAdded++;
            }
        }

        Console.WriteLine(sandAdded + " grains of sand come to rest before they fall into the abyss");
    }

    public void Part2(string input)
    {
        var grid = CreateGridFromInput(input, true);

        var addingSand = true;
        var sandAdded = 0;

        while (addingSand) {
            var result = AddSand(grid);

            if (result == null) {
                addingSand = false;
            } else {
                grid = result;
                sandAdded++;
            }
        }

        Console.WriteLine(sandAdded + " grains of sand come to rest before blocking the source");
    }

    private static char[,] CreateGridFromInput(string input, bool includeFloor = false)
    {
        var seams = input
            .Split("\n", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries)
            .Select(l => l.Split(" -> ")
                .Select(sc => sc
                    .Split(",")
                    .Select(int.Parse))
                .Select(c => (c.ElementAt(0), c.ElementAt(1))));

        var maxY = seams.Select(s => s.Select(sl => sl.Item2).Max()).Max() + 1;

        if (includeFloor) {
            maxY += 2;
        }

        var grid = new char[maxY,1000];

        for (var y = 0; y < maxY; y++) {
            for (var x = 0; x < 1000; x++) {
                if (includeFloor && y == maxY - 1) {
                    grid[y, x] = '#';
                } else {
                    grid[y, x] = '.';
                }
            }
        }

        foreach (var seam in seams) {
            var pairs = Enumerable
                .Range(0, seam.Count() - 1)
                .Aggregate(
                    new List<((int, int), (int, int))>().ToImmutableList(),
                    (acc, n) => acc.Add((
                        (seam.ElementAt(n).Item1, seam.ElementAt(n).Item2),
                        (seam.ElementAt(n + 1).Item1, seam.ElementAt(n + 1).Item2)
                    ))
                );

            foreach (var pair in pairs) {
                for (
                    var y = Math.Min(pair.Item1.Item2, pair.Item2.Item2);
                    y <= Math.Max(pair.Item1.Item2, pair.Item2.Item2);
                    y++
                ) {
                    for (
                        var x = Math.Min(pair.Item1.Item1, pair.Item2.Item1);
                        x <= Math.Max(pair.Item1.Item1, pair.Item2.Item1);
                        x++
                    ) {
                        grid[y, x] = '#';
                    }
                }
            }
        }

        return grid;
    }

    private static char[,]? AddSand(char[,] grid)
    {
        var oldSandPos = (500, -1);
        var currentSandPos = (500, 0);

        while (oldSandPos != currentSandPos) {
            oldSandPos = currentSandPos;

            if (grid[0, 500] == 'o') {
                //Source of sand is blocked
                return null;
            }

            if (currentSandPos.Item2 >= grid.GetLength(0) - 1) {
                //We've fallen off the bottom of the screen
                return null;
            }

            if (grid[currentSandPos.Item2 + 1, currentSandPos.Item1] == '.') {
                //Try to move down
                currentSandPos = (currentSandPos.Item1, currentSandPos.Item2 + 1);
            } else if (grid[currentSandPos.Item2 + 1, currentSandPos.Item1 - 1] == '.') {
                //Try to move down left
                currentSandPos = (currentSandPos.Item1 - 1, currentSandPos.Item2 + 1);
            } else if (grid[currentSandPos.Item2 + 1, currentSandPos.Item1 + 1] == '.') {
                //Try to move down right
                currentSandPos = (currentSandPos.Item1 + 1, currentSandPos.Item2 + 1);
            }
        }

        grid[currentSandPos.Item2, currentSandPos.Item1] = 'o';

        return grid;
    }

    private static void PrintGrid(char[,] grid)
    {
        for (var y = 0; y < grid.GetLength(0); y++) {
            for (var x = 0; x < grid.GetLength(1); x++) {
                Console.Write(grid[y, x]);
            }
            Console.WriteLine();
        }

        Console.WriteLine();
    }
}
