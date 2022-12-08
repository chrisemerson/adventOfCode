namespace AdventOfCode;

public class Day8 : IAdventOfCodeDay
{
    public void Part1(string input) => Console.WriteLine(
        "Visible trees: " + BuildTreeGridFromInput(input)
            .Select((r, y) => r
                .Select((_, x) => IsVisibleFromEdge(x, y, BuildTreeGridFromInput(input)))
                .Count(t => t))
            .Sum());

    public void Part2(string input) => Console.WriteLine(
        "Best scenic score: " + BuildTreeGridFromInput(input)
            .Select((r, y) => r
                .Select((_, x) => GetScenicScoreForTree(x, y, BuildTreeGridFromInput(input)))
                .Max()
            ).Max());

    private static int[][] BuildTreeGridFromInput(string input) => input
        .Split("\n")
        .Where(l => l != "")
        .Select(r => r
            .Select(c => int.Parse(c.ToString()))
            .ToArray())
        .ToArray();

    private static bool IsVisibleFromEdge(int x, int y, IReadOnlyList<int[]> grid) => GetSightLinesFromTree(x, y, grid)
        .Select(r => r.Count(c => grid[c.Item2][c.Item1] >= grid[y][x]))
        .Count(c => c > 0) < 4;

    private static int GetScenicScoreForTree(int x, int y, IReadOnlyList<int[]> grid) =>
        GetSightLinesFromTree(x, y, grid)
            .Select(r => r.Select(t => grid[t.Item2][t.Item1]))
            .Select(r => Math.Min(r.Count(), r.TakeWhile(t => t < grid[y][x]).Count() + 1))
            .Aggregate(1, (a, b) => a * b);

    private static IEnumerable<(int, int)>[] GetSightLinesFromTree(int x, int y, IReadOnlyList<int[]> grid) => new[] {
        Enumerable.Range(0, y).Select(dy => (x, dy)).Reverse(),
        Enumerable.Range(y + 1, grid.Count - y - 1).Select(dy => (x, dy)),
        Enumerable.Range(0, x).Select(dx => (dx, y)).Reverse(),
        Enumerable.Range(x + 1, grid.Count - x - 1).Select(dx => (dx, y))
    };
}
