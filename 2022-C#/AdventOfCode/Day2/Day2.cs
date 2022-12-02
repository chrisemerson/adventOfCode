namespace AdventOfCode;

public class Day2 : IAdventOfCodeDay
{
    public void Part1(string input) => Console.WriteLine(
        ScoreGame(input, new Dictionary<string, string>
        {
            { "A X", "R R" }, { "A Y", "R P" }, { "A Z", "R S" },
            { "B X", "P R" }, { "B Y", "P P" }, { "B Z", "P S" },
            { "C X", "S R" }, { "C Y", "S P" }, { "C Z", "S S" }
        }));

    public void Part2(string input) => Console.WriteLine(
        ScoreGame(input, new Dictionary<string, string>
        {
            { "A X", "R S" }, { "A Y", "R R" }, { "A Z", "R P" },
            { "B X", "P R" }, { "B Y", "P P" }, { "B Z", "P S" },
            { "C X", "S P" }, { "C Y", "S S" }, { "C Z", "S R" }
        }));

    private static int ScoreGame(string input, IReadOnlyDictionary<string, string> moveKey) =>
        input
            .Split("\n")
            .Where(l => l != "")
            .Select(round => moveKey[round])
            .Select(round => round.Split(" "))
            .Select(moves => ScoreRound(moves[0][0], moves[1][0]))
            .Sum();

    private static int ScoreRound(char theirChoice, char ourChoice)
    {
        var score = ourChoice switch
        {
            'R' => 1,
            'P' => 2,
            'S' => 3,
            _ => throw new ArgumentOutOfRangeException(
                nameof(ourChoice),
                ourChoice,
                "ourChoice must be 'R', 'P' or 'S'"
            )
        };

        if (theirChoice == ourChoice) {
            score += 3;
        } else if (WeWin(theirChoice, ourChoice)) {
            score += 6;
        }

        return score;
    }

    private static bool WeWin(char theirChoice, char ourChoice) => theirChoice switch
    {
        'R' => ourChoice == 'P',
        'P' => ourChoice == 'S',
        'S' => ourChoice == 'R',
        _ => throw new ArgumentOutOfRangeException(
            nameof(theirChoice),
            theirChoice,
            "theirChoice must be 'R', 'P' or 'S'"
        )
    };
}
