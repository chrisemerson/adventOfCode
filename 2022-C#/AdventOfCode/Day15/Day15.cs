namespace AdventOfCode;

public class Day15 : IAdventOfCodeDay
{
    public void Part1(string input)
    {
        var sensors = input
            .Split("\n", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries)
            .Select(ParseLine)
            .ToArray();

        var minX = sensors.Select(s => s.Item1.Item1 - s.Item3).Min();
        var maxX = sensors.Select(s => s.Item1.Item1 + s.Item3).Max();

        var coverage = GetCoverage(sensors, 2000000);

        var couldContainBeacon = new Dictionary<int, bool>();

        for (var x = minX; x <= maxX; x++) {
            couldContainBeacon.Add(x, true);
        }

        foreach (var cover in coverage) {
            for (var x = cover.Item1; x <= cover.Item2; x++) {
                couldContainBeacon[x] = false;
            }
        }

        foreach (var sensor in sensors) {
            if (sensor.Item2.Item2 == 2000000) {
                couldContainBeacon[sensor.Item2.Item1] = true;
            }
        }

        Console.WriteLine(couldContainBeacon.Count(s => !s.Value) + " cells cannot contain a beacon");
    }

    public void Part2(string input)
    {
        var sensors = input
            .Split("\n", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries)
            .Select(ParseLine)
            .ToArray();

        const int maxRange = 4000000;

        for (var y = 0; y <= maxRange; y++) {
            var coverage = GetCoverage(sensors, y)
                .Where(r => r.Item2 >= 0 && r.Item1 <= maxRange)
                .Select(r => (Math.Max(r.Item1, 0), Math.Min(r.Item2, maxRange)))
                .OrderBy(r => r.Item1);

            var currentRange = coverage.First();

            foreach (var range in coverage) {
                if (range.Item1 <= currentRange.Item2) {
                    currentRange = (
                        Math.Min(currentRange.Item1, range.Item1),
                        Math.Max(currentRange.Item2, range.Item2)
                    );
                } else {
                    Console.WriteLine("Tuning frequency: " + (((ulong) currentRange.Item2 + 1) * 4000000 + (ulong) y));
                    Environment.Exit(0);
                }
            }
        }
    }

    private static ((int, int), (int, int), int) ParseLine(string line)
    {
        var lineParts = line.Split(": closest beacon is at x=");

        var sensorParts = lineParts[0].Split(", y=");
        var beaconParts = lineParts[1].Split(", y=");

        var sensorLocation = (int.Parse(sensorParts[0][12..]), int.Parse(sensorParts[1]));
        var beaconLocation = (int.Parse(beaconParts[0]), int.Parse(beaconParts[1]));

        return (sensorLocation, beaconLocation, ManhattanDistance(sensorLocation, beaconLocation));
    }

    private static IEnumerable<(int, int)> GetCoverage(
        IEnumerable<((int, int), (int, int), int)> sensors,
        int rowToInspect
    ) => sensors
        .Select(s => (
            s.Item1.Item1 - (s.Item3 - Math.Abs(s.Item1.Item2 - rowToInspect)),
            s.Item1.Item1 + (s.Item3 - Math.Abs(s.Item1.Item2 - rowToInspect))));

    private static int ManhattanDistance((int, int) a, (int, int) b) =>
        Math.Abs(a.Item1 - b.Item1) + Math.Abs(a.Item2 - b.Item2);
}
