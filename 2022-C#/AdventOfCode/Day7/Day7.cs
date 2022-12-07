namespace AdventOfCode;

internal struct FileSystem
{
    public string CurrentDirectory;
    public Dictionary<string, int> Directories = new() { { "/", 0 } };

    public FileSystem(string currentDirectory)
    {
        CurrentDirectory = currentDirectory;
    }

    public FileSystem WithCurrentDirectory(string newCurrentDirectory)
    {
        CurrentDirectory = newCurrentDirectory;
        return this;
    }

    public FileSystem WithDirectories(Dictionary<string, int> newDirectories)
    {
        Directories = newDirectories;
        return this;
    }
}

public class Day7 : IAdventOfCodeDay
{
    private const int TotalSize = 70000000;
    private const int SpaceRequired = 30000000;

    public void Part1(string input) => Console.WriteLine(
        GetFileSystem(input)
            .Directories
            .Where(d => d.Value <= 100000)
            .Select(d => d.Value)
            .Sum());

    public void Part2(string input)
    {
        var fs = GetFileSystem(input);

        var spaceNeeded = (SpaceRequired + fs.Directories["/"]) - TotalSize;

        Console.Write("Space needed: " + spaceNeeded);

        foreach (var entry in fs.Directories) {
            Console.WriteLine(entry.Key + ": " + entry.Value);
        }

        Console.WriteLine(fs
            .Directories
            .Where(d => d.Value >= (SpaceRequired + fs.Directories["/"]) - TotalSize)
            .Min(d => d.Value));
    }

    private static FileSystem GetFileSystem(string input) => input
        .Split("\n")
        .Where(l => l != "")
        .Aggregate(new FileSystem("/"), (fs, l) => ProcessInputLine(fs, l.Split(" ")));

    private static FileSystem ProcessInputLine(FileSystem fs, IReadOnlyList<string> inputLineParts) =>
        inputLineParts.ElementAt(0) switch {
            "$" => ProcessInstruction(fs, inputLineParts.Skip(1)),
            "dir" => fs,
            _ => ProcessFileEntry(fs, int.Parse(inputLineParts.ElementAt(0)))
        };

    private static FileSystem ProcessInstruction(FileSystem fs, IEnumerable<string> instruction) =>
        instruction.ElementAt(0) switch {
            "cd" => ProcessDirectoryChange(fs, instruction.ElementAt(1)),
            _ => fs
        };

    private static FileSystem ProcessDirectoryChange(FileSystem fs, string directory) =>
        directory switch {
            "/" => fs.WithCurrentDirectory("/"),
            ".." => fs.WithCurrentDirectory("/" + string.Join('/', fs.CurrentDirectory.Split("/").SkipLast(1)) + "/"),
            _ => fs.WithCurrentDirectory(fs.CurrentDirectory + directory + "/")
                .WithDirectories(fs.Directories.Replace(fs.CurrentDirectory, 0))
        };

    private static FileSystem ProcessFileEntry(FileSystem fs, int fileSize) => fs.CurrentDirectory
        .Split("/")
        .Where(d => d != "")
        .Aggregate(fs.WithCurrentDirectory("/"), (f, dir) => f
            .WithCurrentDirectory(f.CurrentDirectory + dir + "/")
            .WithDirectories(f.Directories
                .Replace(
                    f.CurrentDirectory,
                    f.Directories[f.CurrentDirectory] + fileSize)));
}
