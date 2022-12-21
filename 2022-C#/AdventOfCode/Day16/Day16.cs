using System.Collections.Immutable;

namespace AdventOfCode;

public class Day16 : IAdventOfCodeDay
{
    public void Part1(string input)
    {
        var bestScore = FindBestMove(new CurrentState("AA", 30, ParseInput(input)), "AA");

        foreach (var move in bestScore.Item2.Reverse()) {
            Console.WriteLine(move);
        }

        Console.WriteLine("Most pressure able to be released: " + bestScore.Item1);
    }

    public void Part2(string input)
    {
        throw new NotImplementedException();
    }

    private static (int, ImmutableList<string>) FindBestMove(CurrentState state, string? lastLocation)
    {
        //If no time left, return 0
        if (state.TimeLeft <= 0) {
            return (0, new List<string> { state.TimeLeft + ": Time out!" }.ToImmutableList());
        }

        var possibleFlows = new List<(int, ImmutableList<string>)>();
        var currentValve = state.Valves[state.CurrentPosition];

        foreach (var nextValve in state.Valves.Where(v => !v.Value.Open && v.Key != state.CurrentPosition && v.Value.FlowRate > 0)) {
            var travelCost = currentValve.Connections[nextValve.Key] + 1;

            if (
                nextValve.Key != lastLocation
                && !state.Valves[nextValve.Key].Open
                && travelCost <= state.TimeLeft
            ) {
                var bestMoveFromHere = FindBestMove(
                    new CurrentState(
                        nextValve.Key,
                        state.TimeLeft - travelCost,
                        state.Valves.Select(v => v.Key == nextValve.Key
                                ? new Valve(v.Value.Name, v.Value.Connections, v.Value.FlowRate, true)
                                : v.Value)
                            .ToDictionary(v => v.Name, v => v)
                    ), state.CurrentPosition
                );

                possibleFlows.Add((
                        bestMoveFromHere.Item1 + state.Valves.Sum(v => v.Value.Open ? v.Value.FlowRate * travelCost : 0),
                        bestMoveFromHere.Item2.Add(state.TimeLeft + ": Move to & open " + nextValve.Key))
                );
            }
        }

        return possibleFlows.Any()
            ? possibleFlows.First(pf => pf.Item1 == possibleFlows.Select(pf2 => pf2.Item1).Max())
            : (state.Valves
                    .Where(v => v.Value.Open)
                    .Sum(v => v.Value.FlowRate) * state.TimeLeft,
                new List<string> { "No further possible moves" }.ToImmutableList()
            );
    }

    private static Dictionary<string, Valve> ParseInput(string input)
    {
        var valves = input
            .Split("\n", StringSplitOptions.RemoveEmptyEntries)
            .Select(l =>
            {
                var valveParts = l.Split("; ");
                var valveInfo = valveParts[0].Split(" has flow rate=");

                return new Valve(
                    valveInfo[0][6..],
                    valveParts[1].StartsWith("tunnel ")
                        ? valveParts[1][22..].Split(",", StringSplitOptions.TrimEntries).ToDictionary(v => v, _ => 1)
                        : valveParts[1][23..].Split(",", StringSplitOptions.TrimEntries).ToDictionary(v => v, _ => 1),
                    int.Parse(valveInfo[1])
                );
            });

        var valveDistances = new Dictionary<(string, string), int>();

        var keyValves = valves.Where(v => v.FlowRate > 0 || v.Name == "AA");

        foreach (var valveA in keyValves) {
            foreach (var valveB in keyValves) {
                if (valveA.Name != valveB.Name) {
                    valveDistances.Add((valveA.Name, valveB.Name),
                        FindShortestDistanceBetweenValves(valves, valveA.Name, valveB.Name,
                            new List<string>().ToImmutableList()));
                }
            }
        }

        return valves
            .Where(v => keyValves.Select(kv => kv.Name).Contains(v.Name))
            .Select(v => new Valve(
                v.Name,
                valveDistances
                    .Where(vd => vd.Key.Item1 == v.Name)
                    .Where(vd => keyValves.Select(kv => kv.Name).Contains(vd.Key.Item2))
                    .ToDictionary(vd => vd.Key.Item2, vd => vd.Value),
                v.FlowRate,
                v.Open))
            .ToDictionary(v => v.Name, v => v);
    }

    private static int FindShortestDistanceBetweenValves(
        IEnumerable<Valve> valves,
        string valveA,
        string valveB,
        ImmutableList<string> visited
    ) {
        if (valveA == valveB) {
            return 0;
        }

        var possibleDestinations = valves
            .First(v => v.Name == valveA)
            .Connections.Where(d => !visited.Contains(d.Key));

        return possibleDestinations.Any()
            ? possibleDestinations
                .Select(d => FindShortestDistanceBetweenValves(valves, d.Key, valveB, visited.Add(valveA)))
                .Min() + 1
            : 9999;
    }
}
