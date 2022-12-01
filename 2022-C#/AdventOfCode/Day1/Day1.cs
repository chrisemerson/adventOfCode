namespace AdventOfCode;

public class Day1: IAdventOfCodeDay
{
    public void Part1(string input) =>
        Console.WriteLine("Max calories of any elf is: " + GetCaloriesByElf(input).Max());

    public void Part2(string input) =>
        Console.WriteLine("Max calories of top 3 elves is: " + GetCaloriesByElf(input).OrderDescending().Take(3).Sum());

    private static IEnumerable<int> GetCaloriesByElf(string input) => input.Split("\n\n")
        .Select(elf => elf
            .Split("\n")
            .Where(calorie => calorie != "")
            .Select(int.Parse)
            .Sum())
        .ToList();
}
