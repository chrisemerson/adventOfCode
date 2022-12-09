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
                ((0, 0), (0, 0), new HashSet<(int, int)> { (0, 0) }.ToImmutableHashSet()),
                (acc, x) => MakeMove(acc, x.ElementAt(0)[0], int.Parse(x.ElementAt(1))))
            .Item3
            .Count
        + " positions");

    public void Part2(string input) => Console.WriteLine(
        "Tail has visited "
        + input
            .Split("\n", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
            .Select(l => l.Split(" "))
            .Aggregate(
                (
                    (0, 0),
                    new List<(int, int)> {
                        (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)
                    }.ToImmutableList(),
                    new HashSet<(int, int)> { (0, 0) }.ToImmutableHashSet()
                ),
                (state, instruction) => Enumerable
                    .Repeat(instruction.ElementAt(0)[0], int.Parse(instruction.ElementAt(1)))
                    .Aggregate(state, (iState, iInstruction) =>
                        new List<((int, int), ImmutableList<(int, int)>, ImmutableHashSet<(int, int)>)>
                                { iState }
                            .Select(x => (x, GetNextHeadPosition(x.Item1, iInstruction, 1)))
                            .Select(x => (x.Item1.Item2.Aggregate((
                                    x.Item2,
                                    new List<(int, int)>().ToImmutableList(),
                                    new HashSet<(int, int)>().ToImmutableHashSet()),
                                (iiState, nextTail) => new List<(
                                        ((int, int), ImmutableList<(int, int)>, ImmutableHashSet<(int, int)>),
                                        (int, int),
                                        ((int, int), (int, int), ImmutableHashSet<(int, int)>)
                                        )> {
                                        (
                                            iiState,
                                            nextTail,
                                            UpdateTailPositionAndVisitedCells(
                                                iiState.Item1,
                                                nextTail,
                                                new HashSet<(int, int)> { nextTail }.ToImmutableHashSet()
                                            ))
                                    }
                                    .Select(x => (x.Item3.Item2, iiState.Item2.Add(x.Item3.Item2), x.Item3.Item3))
                                    .First()), x.Item2))
                            .Select(x => (
                                x.Item2,
                                x.Item1.Item2,
                                x.Item1.Item3.Aggregate(iState.Item3, (acc, item) => acc.Add(item))
                            ))
                            .First()
                    )
            )
            .Item3.Count
        + " positions");

    private static ((int, int), (int, int), ImmutableHashSet<(int, int)>) MakeMove(
        ((int, int), (int, int), ImmutableHashSet<(int, int)>) ropeState,
        char dir,
        int dist
    ) => UpdateTailPositionAndVisitedCells(
        GetNextHeadPosition(ropeState.Item1, dir, dist),
        ropeState.Item2,
        ropeState.Item3);

    private static (int, int) GetNextHeadPosition((int, int) headPos, char dir, int dist) => (
        headPos.Item1 + dist * MoveMap[dir].Item1,
        headPos.Item2 + dist * MoveMap[dir].Item2
    );

    private static ((int, int), (int, int), ImmutableHashSet<(int, int)>) UpdateTailPositionAndVisitedCells(
        (int, int) headPos,
        (int, int) tailPos,
        ImmutableHashSet<(int, int)> visited
    ) => Math.Abs(headPos.Item1 - tailPos.Item1) <= 1 && Math.Abs(headPos.Item2 - tailPos.Item2) <= 1
        ? (headPos, tailPos, visited)
        : UpdateTailPositionAndVisitedCells(
            headPos,
            GetNextTailPosition(headPos, tailPos),
            visited.Add(GetNextTailPosition(headPos, tailPos)));

    private static (int, int) GetNextTailPosition((int, int) headPos, (int, int) tailPos) => (
        tailPos.Item1 == headPos.Item1
            ? tailPos.Item1
            : tailPos.Item1 + (headPos.Item1 - tailPos.Item1) / Math.Abs(headPos.Item1 - tailPos.Item1),
        tailPos.Item2 == headPos.Item2
            ? tailPos.Item2
            : tailPos.Item2 + (headPos.Item2 - tailPos.Item2) / Math.Abs(headPos.Item2 - tailPos.Item2));
}
