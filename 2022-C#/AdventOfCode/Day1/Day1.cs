namespace AdventOfCode;

public class Day1: IAdventOfCodeDay
{
    public void Part1(string input) =>
        Console.WriteLine("Max calories of any elf is: " + getCaloriesByElf(input).Max());

    public void Part2(string input) =>
        Console.WriteLine("Max calories of top 3 elves is: " + getCaloriesByElf(input).OrderDescending().Take(3).Sum());

    private List<int> getCaloriesByElf(string input) => input.Split("\n\n")
        .Select(elf => elf
            .Split("\n")
            .Where(calorie => calorie != "")
            .Select(calorie => int.Parse(calorie))
            .Sum())
        .ToList();
}
