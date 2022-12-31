using System.Collections.Immutable;

namespace AdventOfCode;

public class Day16 : IAdventOfCodeDay
{
    public void Part1(string input) => Console.WriteLine(
        "Most pressure able to be released: "
        + FindBestMove(new CurrentState(("AA", 0), 30, ParseInput(input))));

    public void Part2(string input) => Console.WriteLine(
        "Most pressure able to be released: "
        + FindBestMove(new CurrentState(("AA", 0), 26, ParseInput(input), ("AA", 0))));

    private static int FindBestMove(CurrentState state)
    {
        //If no time left, return 0
        if (state.TimeLeft <= 0) {
            return 0;
        }

        var possibleFlows = new List<int>();

        if (state.ElephantPosition.HasValue) {
            //Elephant on board
            var possibleRouting = new List<(string, string)>();
            var weCanMove = state.CurrentPosition.Item2 == 0;
            var elephantCanMove = state.ElephantPosition.Value.Item2 == 0;
            var availableValves = state.Valves.Where(v => !v.Value.Open && v.Value.FlowRate > 0);

            if (weCanMove && elephantCanMove) {
                //We need to choose new valves for both us and the elephant to visit
                foreach (var nextValve in availableValves) {
                    foreach (var elephantNextValve in availableValves) {
                        if (nextValve.Key != elephantNextValve.Key) {
                            possibleRouting.Add((nextValve.Key, elephantNextValve.Key));
                        }
                    }
                }
            } else {
                //One of us is on a pre-determined route, but we still need to choose somewhere for the other to visit
                foreach (var nextValve in availableValves) {
                    possibleRouting.Add(weCanMove
                        ? (nextValve.Key, state.ElephantPosition.Value.Item1)
                        : (state.CurrentPosition.Item1, nextValve.Key));
                }
            }

            foreach (var possibleRoute in possibleRouting) {
                var myDestination = possibleRoute.Item1;
                var elephantDestination = possibleRoute.Item2;

                var myTravelCost = weCanMove
                    ? state.Valves[state.CurrentPosition.Item1].Connections[myDestination] + 1
                    : state.CurrentPosition.Item2;

                var elephantTravelCost = elephantCanMove
                    ? state.Valves[state.ElephantPosition.Value.Item1].Connections[elephantDestination] + 1
                    : state.ElephantPosition.Value.Item2;

                if (myTravelCost <= state.TimeLeft || elephantTravelCost <= state.TimeLeft) {
                    var minTravelCost = Math.Min(myTravelCost, elephantTravelCost);
                    Dictionary<string, Valve> newValves;

                    if (myTravelCost == elephantTravelCost) {
                        newValves = state.Valves.Select(v => (v.Key == myDestination || v.Key == elephantDestination)
                                ? new Valve(v.Value.Name, v.Value.Connections, v.Value.FlowRate, true)
                                : v.Value)
                            .ToDictionary(v => v.Name, v => v);
                    } else if (myTravelCost < elephantTravelCost) {
                        newValves = state.Valves.Select(v => v.Key == myDestination
                                ? new Valve(v.Value.Name, v.Value.Connections, v.Value.FlowRate, true)
                                : v.Value)
                            .ToDictionary(v => v.Name, v => v);
                    } else {
                        newValves = state.Valves.Select(v => v.Key == elephantDestination
                                ? new Valve(v.Value.Name, v.Value.Connections, v.Value.FlowRate, true)
                                : v.Value)
                            .ToDictionary(v => v.Name, v => v);
                    }

                    var bestMoveFromHere = FindBestMove(new CurrentState(
                        (myDestination, myTravelCost - minTravelCost),
                        state.TimeLeft - minTravelCost,
                        newValves,
                        (elephantDestination, elephantTravelCost - minTravelCost)
                    ));

                    possibleFlows.Add(bestMoveFromHere + state.Valves.Sum(v => v.Value.Open
                        ? v.Value.FlowRate * minTravelCost
                        : 0));
                }
            }
        } else {
            foreach (var nextValve in state.Valves
                         .Where(v => !v.Value.Open && v.Key != state.CurrentPosition.Item1 && v.Value.FlowRate > 0)) {
                var myTravelCost = state.Valves[state.CurrentPosition.Item1].Connections[nextValve.Key] + 1;

                if (myTravelCost <= state.TimeLeft) {
                    var bestMoveFromHere = FindBestMove(
                        new CurrentState(
                            (nextValve.Key, 0),
                            state.TimeLeft - myTravelCost,
                            state.Valves.Select(v => v.Key == nextValve.Key
                                    ? new Valve(v.Value.Name, v.Value.Connections, v.Value.FlowRate, true)
                                    : v.Value)
                                .ToDictionary(v => v.Name, v => v)
                        )
                    );

                    possibleFlows.Add(bestMoveFromHere + state.Valves.Sum(v => v.Value.Open
                        ? v.Value.FlowRate * myTravelCost
                        : 0));
                }
            }
        }

        return possibleFlows.Any()
            ? possibleFlows.Last(pf => pf == possibleFlows.Select(pf2 => pf2).Max())
            : state.Valves
                .Where(v => v.Value.Open)
                .Sum(v => v.Value.FlowRate) * state.TimeLeft;
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
                        ? valveParts[1][22..].Split(",", StringSplitOptions.TrimEntries)
                            .ToDictionary(v => v, _ => 1)
                        : valveParts[1][23..].Split(",", StringSplitOptions.TrimEntries)
                            .ToDictionary(v => v, _ => 1),
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
