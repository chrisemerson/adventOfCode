namespace AdventOfCode;

public class Day4 : IAdventOfCodeDay
{
    public void Part1(string input) => RunCheck(input, RangesFullyContained);

    public void Part2(string input) => RunCheck(input, RangesOverlap);

    private static void RunCheck(string input, Func<int, int, int, int, bool> checkCallback) => Console.WriteLine(input
        .Split("\n")
        .Where(l => l != "")
        .Select(l => l
            .Split(",")
            .Select(r => r
                .Split("-")
                .Select(int.Parse)
                .ToArray())
            .ToArray())
        .Count(ranges => checkCallback(ranges[0][0], ranges[0][1], ranges[1][0], ranges[1][1])));

    private static bool RangesFullyContained(int aMin, int aMax, int bMin, int bMax) =>
        (aMin >= bMin && aMax <= bMax) || (bMin >= aMin && bMax <= aMax);

    private static bool RangesOverlap(int aMin, int aMax, int bMin, int bMax) =>
        (aMin >= bMin && aMin <= bMax) || (bMin >= aMin && bMin <= aMax);
}
