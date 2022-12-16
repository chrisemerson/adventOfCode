namespace AdventOfCode;

public class Day13 : IAdventOfCodeDay
{
    public void Part1(string input) => Console.WriteLine("Sum of indices of in-order packets: " + input
        .Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
        .Select((p, i) => new { p, i })
        .Where(p => PacketsInOrder(p.p))
        .Aggregate(0, (acc, p) => acc + p.i + 1));

    public void Part2(string input)
    {
        var packets = input
            .Split("\n", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
            .ToList();

        packets.Add("[[2]]");
        packets.Add("[[6]]");
        packets.Sort(CompareItems);

        Console.WriteLine(
            "Multiple of indices of divider packets: " + packets
                .Select((p, i) => new { p, i })
                .Where(p => p.p is "[[2]]" or "[[6]]")
                .Aggregate(1, (acc, p) => acc * (p.i + 1))
        );
    }

    private static bool PacketsInOrder(string s)
    {
        var packets = s.Split("\n", StringSplitOptions.TrimEntries);

        return CompareItems(packets[0], packets[1]) == -1;
    }

    private static int CompareItems(string left, string right)
    {
        var leftIsList = left.Contains(",") || left.Contains("[");
        var rightIsList = right.Contains(",") || right.Contains("[");

        if (leftIsList && !rightIsList) {
            right = "[" + right + "]";
        }

        if (!leftIsList && rightIsList) {
            left = "[" + left + "]";
        }

        if (leftIsList || rightIsList) {
            var leftItems = ParseString(left);
            var rightItems = ParseString(right);

            for (var i = 0; i < Math.Min(leftItems.Count(), rightItems.Count()); i++) {
                var comparison = CompareItems(leftItems.ElementAt(i), rightItems.ElementAt(i));

                if (comparison != 0) {
                    return comparison;
                }
            }

            if (leftItems.Count() != rightItems.Count()) {
                return (leftItems.Count() - rightItems.Count()) / Math.Abs(leftItems.Count() - rightItems.Count());
            }

            return 0;
        }

        if (int.Parse(left) == int.Parse(right)) {
            return 0;
        }

        return (int.Parse(left) - int.Parse(right)) / Math.Abs(int.Parse(left) - int.Parse(right));
    }

    private static IEnumerable<string> ParseString(string packet)
    {
        var level = 0;
        var items = new List<string>();

        var tempString = "";

        foreach (var c in packet) {
            tempString += c;

            switch (c) {
                case '[':
                    level++;
                    break;

                case ']':
                    level--;

                    if (level == 0) {
                        items.Add(tempString);
                        tempString = "";
                    }

                    break;

                case ',':
                    if (level == 0) {
                        if (tempString.Length > 1) {
                            items.Add(tempString[..^1]);
                        }

                        tempString = "";
                    }

                    break;
            }
        }

        items.Add(tempString);

        if (
            items.ElementAt(0).Length == packet.Length
            && packet[0] == '['
            && packet[^1] == ']'
        ) {
            return new List<string> { packet[1..^1] }.Where(x => x != "");
        }

        return items.Where(x => x != "");
    }
}
