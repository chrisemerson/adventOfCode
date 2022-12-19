namespace AdventOfCode;

public class Day16 : IAdventOfCodeDay
{
    public void Part1(string input)
    {
        var valves = ParseInput(input);
    }

    public void Part2(string input)
    {
        throw new NotImplementedException();
    }

    private static IEnumerable<(string, int, string[])> ParseInput(string input)
    {
        return input
            .Split("\n", StringSplitOptions.RemoveEmptyEntries)
            .Select(l =>
            {
                var valveParts = l.Split("; ");
                var valveInfo = valveParts[0].Split(" has flow rate=");

                return (
                    valveInfo[0].Substring(6),
                    int.Parse(valveInfo[1]),
                    valveParts[1].StartsWith("tunnel ")
                        ? valveParts[1][22..].Split(",", StringSplitOptions.TrimEntries)
                        : valveParts[1][23..].Split(",", StringSplitOptions.TrimEntries)
                );
            });
    }
}
