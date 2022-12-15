using System.Net;

namespace AdventOfCode;

public class Day12 : IAdventOfCodeDay
{
    public void Part1(string input)
    {
        var (grid, stepsGrid, start, end) = InitialiseFromInput(input);

        stepsGrid[start.Item1][start.Item2] = 0;

        while (stepsGrid[end.Item1][end.Item2] == -1) {
            var maxSteps = stepsGrid.Select(x => x.Max()).Max();

            for (var i = 0; i < grid.Length; i++) {
                for (var j = 0; j < grid[i].Length; j++) {
                    if (stepsGrid[i][j] == maxSteps) {
                        GetAvailableStepsUp(i, j, grid)
                            .ToList()
                            .ForEach(c => stepsGrid[c.Item1][c.Item2] = stepsGrid[c.Item1][c.Item2] == -1
                                ? maxSteps + 1
                                : Math.Min(stepsGrid[c.Item1][c.Item2], maxSteps + 1));
                    }
                }
            }
        }

        Console.WriteLine("Shortest path to end is " + stepsGrid[end.Item1][end.Item2] + " steps");
    }

    public void Part2(string input)
    {
        var (grid, stepsGrid, start, end) = InitialiseFromInput(input);

        stepsGrid[end.Item1][end.Item2] = 0;

        while (stepsGrid[start.Item1][start.Item2] == -1) {
            var maxSteps = stepsGrid.Select(x => x.Max()).Max();

            for (var i = 0; i < grid.Length; i++) {
                for (var j = 0; j < grid[i].Length; j++) {
                    if (stepsGrid[i][j] == maxSteps) {
                        GetAvailableStepsDown(i, j, grid)
                            .ToList()
                            .ForEach(c => stepsGrid[c.Item1][c.Item2] = stepsGrid[c.Item1][c.Item2] == -1
                                ? maxSteps + 1
                                : Math.Min(stepsGrid[c.Item1][c.Item2], maxSteps + 1));
                    }
                }
            }
        }

        var minSteps = stepsGrid[start.Item1][start.Item2];

        for (var i = 0; i < grid.Length; i++) {
            for (var j = 0; j < grid.Length; j++) {
                if (grid[i][j] == 'a' && stepsGrid[i][j] != -1) {
                    minSteps = Math.Min(minSteps, stepsGrid[i][j]);
                }
            }
        }

        Console.WriteLine("Shortest path from any a to end is " + minSteps + " steps");
    }

    private static (char[][], int[][], (int, int), (int, int)) InitialiseFromInput(string input)
    {
        var grid = input.Split("\n", StringSplitOptions.RemoveEmptyEntries).Select(x => x.ToArray()).ToArray();

        var start = (0, 0);
        var end = (0, 0);

        var stepsGrid = new int[grid.Length][];

        for (var i = 0; i < grid.Length; i++) {
            stepsGrid[i] = new int[grid[i].Length];

            for (var j = 0; j < grid[i].Length; j++) {
                if (grid[i][j] == 'S') {
                    start = (i, j);
                }

                if (grid[i][j] == 'E') {
                    end = (i, j);
                }

                stepsGrid[i][j] = -1;
            }
        }

        grid[start.Item1][start.Item2] = 'a';
        grid[end.Item1][end.Item2] = 'z';

        return (grid, stepsGrid, start, end);
    }

    private IEnumerable<(int, int)> GetAvailableStepsUp(int y, int x, char[][] grid) =>
        new List<(int, int)> { (0, 1), (1, 0), (0, -1), (-1, 0) }
            .Select(d => (y + d.Item1, x + d.Item2))
            .Where(d => d.Item1 >= 0 && d.Item1 < grid.Length && d.Item2 >= 0 && d.Item2 < grid[0].Length)
            .Where(d => grid[d.Item1][d.Item2] - grid[y][x] <= 1);

    private IEnumerable<(int, int)> GetAvailableStepsDown(int y, int x, char[][] grid) =>
        new List<(int, int)> { (0, 1), (1, 0), (0, -1), (-1, 0) }
            .Select(d => (y + d.Item1, x + d.Item2))
            .Where(d => d.Item1 >= 0 && d.Item1 < grid.Length && d.Item2 >= 0 && d.Item2 < grid[0].Length)
            .Where(d => grid[y][x] - grid[d.Item1][d.Item2] <= 1);
}
