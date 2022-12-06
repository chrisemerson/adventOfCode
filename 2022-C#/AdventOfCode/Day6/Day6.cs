namespace AdventOfCode;

public class Day6: IAdventOfCodeDay
{
    public void Part1(string input) => FindFirstSetOfNDifferentCharacters(input, 4);
    public void Part2(string input) => FindFirstSetOfNDifferentCharacters(input, 14);

    private static void FindFirstSetOfNDifferentCharacters(string input, int count)
    {
        for (var i = count - 1; i < input.Length; i++) {
            if (input.Substring(i - count + 1, count).ToHashSet().Count == count) {
                Console.WriteLine("First marker after " + (i + 1) + " characters");
                break;
            }
        }
    }
}
