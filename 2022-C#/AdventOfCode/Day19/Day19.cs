using System.Text.RegularExpressions;

namespace AdventOfCode;

public class Day19 : IAdventOfCodeDay
{
    public void Part1(string input) => Console.WriteLine(
        ParseInput(input)
            .Select(b => new { i = b.Key, q = ScoreBluePrint(b.Value, 24) })
            .Select(b => b.i * b.q).Sum());

    public void Part2(string input) => Console.WriteLine(
        ParseInput(input)
            .Take(3)
            .Select(b => ScoreBluePrint(b.Value, 32))
            .Aggregate(1, (acc, b) => acc * b));

    private static Dictionary<int, Blueprint> ParseInput(string input)
    {
        return input
            .Split("\n", StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries)
            .Select(b =>
            {
                var blueprint = new Blueprint();
                var regex = new Regex(
                    @"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian."
                );

                var match = regex.Match(b);
                var captures = Enumerable
                    .Range(1, 7)
                    .Select(i => new { i, m = int.Parse(match.Groups[i].Captures[0].Value) })
                    .ToDictionary(m => m.i, m => m.m);

                blueprint.id = captures[1];
                blueprint.OreRobot = new Cost(captures[2], 0, 0);
                blueprint.ClayRobot = new Cost(captures[3], 0, 0);
                blueprint.ObsidianRobot = new Cost(captures[4], captures[5], 0);
                blueprint.GeodeRobot = new Cost(captures[6], 0, captures[7]);

                return blueprint;
            })
            .ToDictionary(b => b.id, b => b);
    }

    private static int ScoreBluePrint(Blueprint blueprint, int time)
    {
        var initialState = new State(blueprint);

        //All we can do for the first few moves is collect ore
        var minOreForRobot = Math.Min(blueprint.OreRobot.Ore, blueprint.ClayRobot.Ore);
        initialState.Ore = minOreForRobot;
        initialState.MinutesLeft = time - minOreForRobot;

        return FindBestMove(initialState);
    }

    private static int FindBestMove(State state)
    {
        var options = state.GetOptions();

        if (options.Count == 0 || state.MinutesLeft == 0) {
            return state.Geodes;
        }

        var maxGeodes = 0;

        foreach (var option in options.Where(option => option.IsAchievable(maxGeodes))) {
            maxGeodes = Math.Max(maxGeodes, FindBestMove(option.AdvanceTime()));
        }

        return maxGeodes;
    }
}
