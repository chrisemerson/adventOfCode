namespace AdventOfCode;

public class Day20 : IAdventOfCodeDay
{
    public void Part1(string input) => PrintCoordinates(MixList(ParseInput(input), 1));
    public void Part2(string input) => PrintCoordinates(MixList(ApplyDecryptionKey(ParseInput(input)), 10));

    private static List<(long, int)> ParseInput(string input)
    {
        var list = new List<(long, int)>();
        var index = 0;

        foreach (var num in input.Split("\n", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries)) {
            list.Add((int.Parse(num), index));
            index++;
        }

        return list;
    }

    private static List<(long, int)> MixList(List<(long, int)> list, int times)
    {
        for (var t = 0; t < times; t++) {
            for (var index = 0; index < list.Count; index++) {
                var currentPosition = list.FindIndex(i => i.Item2 == index);
                var itemToMove = list[currentPosition];

                list.RemoveAt(currentPosition);

                var newPosition = (currentPosition + itemToMove.Item1) % list.Count;

                while (newPosition < 0) {
                    newPosition += list.Count;
                }

                list.Insert((int) newPosition, itemToMove);
            }
        }

        return list;
    }

    private static void PrintCoordinates(List<(long, int)> list)
    {
        var indexOfZero = list.FindIndex(i => i.Item1 == 0);

        var coordinates = list[(indexOfZero + 1000) % list.Count].Item1
                          + list[(indexOfZero + 2000) % list.Count].Item1
                          + list[(indexOfZero + 3000) % list.Count].Item1;

        Console.WriteLine("Grove coordinates: " + coordinates);
    }

    private static List<(long, int)> ApplyDecryptionKey(List<(long, int)> list)
    {
        return list.Select(i => ((long) i.Item1 * 811589153, i.Item2)).ToList();
    }
}
