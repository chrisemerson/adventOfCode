using System.Collections.Immutable;

namespace AdventOfCode;

public class Day9 : IAdventOfCodeDay
{
    private static readonly Dictionary<char, (int, int)> MoveMap = new() {
        { 'U', (0, -1) },
        { 'D', (0, 1) },
        { 'L', (-1, 0) },
        { 'R', (1, 0) }
    };

    public void Part1(string input) => Console.Write(
        "Tail has visited "
        + input
            .Split("\n", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
            .Select(l => l.Split(" "))
            .Aggregate(
                ((0, 0), ((0, 0), new HashSet<(int, int)>() { (0, 0) }.ToImmutableHashSet())),
                (acc, x) => MakeMove(acc, x.ElementAt(0)[0], int.Parse(x.ElementAt(1))))
            .Item2.Item2
            .Count
        + " positions");

    public void Part2(string input)
    {
        // input = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2";

        throw new NotImplementedException();
    }

    private static ((int, int), ((int, int), ImmutableHashSet<(int, int)>)) MakeMove(
        ((int, int), ((int, int), ImmutableHashSet<(int, int)>)) ropeState,
        char dir,
        int dist
    ) => (
        UpdateHeadPosition(ropeState.Item1, dir, dist),
        UpdateTailPosition(UpdateHeadPosition(ropeState.Item1, dir, dist), ropeState.Item2.Item1,
            ropeState.Item2.Item2));

    private static (int, int) UpdateHeadPosition((int, int) headPos, char dir, int dist) => (
        headPos.Item1 + dist * MoveMap[dir].Item1,
        headPos.Item2 + dist * MoveMap[dir].Item2
    );

    private static ((int, int), ImmutableHashSet<(int, int)>) UpdateTailPosition(
        (int, int) headPos,
        (int, int) tailPos,
        ImmutableHashSet<(int, int)> visited
    ) => Math.Abs(headPos.Item1 - tailPos.Item1) <= 1 && Math.Abs(headPos.Item2 - tailPos.Item2) <= 1
        ? (tailPos, visited)
        : UpdateTailPosition(
            headPos,
            GetTailMovement(headPos, tailPos),
            visited.Add(GetTailMovement(headPos, tailPos)));

    private static (int, int) GetTailMovement((int, int) headPos, (int, int) tailPos) => (
        tailPos.Item1 == headPos.Item1
            ? tailPos.Item1
            : tailPos.Item1 + (headPos.Item1 - tailPos.Item1) / Math.Abs(headPos.Item1 - tailPos.Item1),
        tailPos.Item2 == headPos.Item2
            ? tailPos.Item2
            : tailPos.Item2 + (headPos.Item2 - tailPos.Item2) / Math.Abs(headPos.Item2 - tailPos.Item2));
}
