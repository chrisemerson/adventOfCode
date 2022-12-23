using System.Collections.Immutable;

namespace AdventOfCode;

public class Day22 : IAdventOfCodeDay
{
    public void Part1(string input) => TraverseMaze(input, MoveForwardOneStepWrap);
    public void Part2(string input) => TraverseMaze(input, MoveForwardOneStepCube);

    private static void TraverseMaze(
        string input,
        Func<ImmutableDictionary<(int, int), char>, (int, int), int, ((int, int), int)> movementFunc
    ) {
        var (instructions, board) = ParseInput(input);

        var currentPosition = (1, board.Where(kv => kv.Key.Item1 == 1).Select(kv => kv.Key.Item2).Min());
        var facing = 0;

        while (instructions.Length != 0) {
            var instruction = GetNextInstruction(instructions);

            switch (instruction) {
                case "L":
                    facing = (facing + 3) % 4;
                    break;

                case "R":
                    facing = (facing + 1) % 4;
                    break;

                default:
                    (currentPosition, facing) = MoveForwardSteps(
                        board,
                        currentPosition,
                        facing,
                        int.Parse(instruction),
                        movementFunc
                    );
                    break;
            }

            instructions = instructions[instruction.Length..];
        }

        Console.WriteLine("Password: " + (currentPosition.Item1 * 1000 + currentPosition.Item2 * 4 + facing));
    }

    private static (string, ImmutableDictionary<(int, int), char>) ParseInput(string input)
    {
        var inputParts = input.Split("\n\n", StringSplitOptions.RemoveEmptyEntries);

        return (
            inputParts[1],
            inputParts[0]
                .Split("\n", StringSplitOptions.RemoveEmptyEntries)
                .Select((l, i) => new {
                    i = i + 1,
                    l = l.Select((c, ii) => new { i = ii + 1, c }).Where(c => c.c != ' ')
                })
                .Aggregate(
                    new Dictionary<(int, int), char>().ToImmutableDictionary(),
                    (acc, l) => l.l
                        .Aggregate(
                            acc,
                            (accInner, c) => accInner.Add((l.i, c.i), c.c))));
    }

    private static string GetNextInstruction(string instructions)
    {
        var instruction = instructions[0].ToString();

        switch (instruction) {
            case "L":
                return "L";

            case "R":
                return "R";

            default:
                var instructionPointer = 1;

                while (
                    instructionPointer < instructions.Length
                    && instructions[instructionPointer] != 'L'
                    && instructions[instructionPointer] != 'R'
                ) {
                    instruction += instructions[instructionPointer];
                    instructionPointer++;
                }

                return instruction;
        }
    }

    private static ((int, int), int) MoveForwardSteps(
        ImmutableDictionary<(int, int), char> board,
        (int, int) currentPosition,
        int facing,
        int steps,
        Func<ImmutableDictionary<(int, int), char>, (int, int), int, ((int, int), int)> movementFunc
    ) {
        var newPosition = currentPosition;

        for (var s = 0; s < steps; s++) {
            (newPosition, facing) = movementFunc(board, newPosition, facing);
        }

        return (newPosition, facing);
    }

    private static ((int, int), int) MoveForwardOneStepWrap(
        ImmutableDictionary<(int, int), char> board,
        (int, int) currentPosition,
        int facing
    ) {
        var newPosition = currentPosition;

        do {
            var boardHeight = board.Select(c => c.Key.Item1).Max();
            var boardWidth = board.Select(c => c.Key.Item2).Max();

            newPosition = facing switch {
                0 => (newPosition.Item1, newPosition.Item2 % boardWidth + 1),
                1 => (newPosition.Item1 % boardHeight + 1, newPosition.Item2),
                2 => (newPosition.Item1, (newPosition.Item2 + boardWidth - 2) % boardWidth + 1),
                3 => ((newPosition.Item1 + boardHeight - 2) % boardHeight + 1, newPosition.Item2),
                _ => newPosition
            };
        } while (!board.ContainsKey(newPosition));

        return (board[newPosition] == '#' ? currentPosition : newPosition, facing);
    }

    private static ((int, int), int) MoveForwardOneStepCube(
        ImmutableDictionary<(int, int), char> board,
        (int, int) currentPosition,
        int facing
    ) {
        var newPosition = facing switch {
            0 => (currentPosition.Item1, currentPosition.Item2 + 1),
            1 => (currentPosition.Item1 + 1, currentPosition.Item2),
            2 => (currentPosition.Item1, currentPosition.Item2 - 1),
            3 => (currentPosition.Item1 - 1, currentPosition.Item2),
            _ => currentPosition
        };

        var newFacing = facing;

        if (newPosition.Item1 == 0 && newPosition.Item2 is >= 101 and <= 150) {
            newPosition = (200, newPosition.Item2 - 100);
        } else if (newPosition.Item1 == 201) {
            newPosition = (1, newPosition.Item2 + 100);
        } else if (newPosition.Item1 == 0) {
            newPosition = (newPosition.Item2 + 100, 1);
            newFacing = 0;
        } else if (newPosition.Item2 == 0 && newPosition.Item1 is >= 151 and <= 200) {
            newPosition = (1, newPosition.Item1 - 100);
            newFacing = 1;
        } else if (newPosition.Item2 == 0 && newPosition.Item1 >= 101 && newPosition.Item1 <= 150) {
            newPosition = (151 - newPosition.Item1, 51);
            newFacing = 0;
        } else if (newPosition.Item2 == 50 && newPosition.Item1 is >= 1 and <= 50) {
            newPosition = (151 - newPosition.Item1, 1);
            newFacing = 0;
        } else if (newPosition.Item1 == 100 && newPosition.Item2 is >= 1 and <= 50 && facing == 3) {
            newPosition = (newPosition.Item2 + 50, 51);
            newFacing = 0;
        } else if (newPosition.Item2 == 50 && newPosition.Item1 is >= 51 and <= 100 && facing == 2) {
            newPosition = (101, newPosition.Item1 - 50);
            newFacing = 1;
        } else if (newPosition.Item1 == 51 && newPosition.Item2 >= 101 && facing == 1) {
            newPosition = (newPosition.Item2 - 50, 100);
            newFacing = 2;
        } else if (newPosition.Item2 == 101 && newPosition.Item1 is >= 51 and <= 100 && facing == 0) {
            newPosition = (50, newPosition.Item1 + 50);
            newFacing = 3;
        } else if (newPosition.Item2 == 151) {
            newPosition = (151 - newPosition.Item1, 100);
            newFacing = (facing + 2) % 4;
        } else if (newPosition.Item2 == 101 && newPosition.Item1 is >= 101 and <= 150) {
            newPosition = (151 - newPosition.Item1, 150);
            newFacing = (facing + 2) % 4;
        } else if (newPosition.Item1 == 151 && newPosition.Item2 is >= 51 and <= 100 && facing == 1) {
            newPosition = (newPosition.Item2 + 100, 50);
            newFacing = 2;
        } else if (newPosition.Item2 == 51 && newPosition.Item1 is >= 151 and <= 200 && facing == 0) {
            newPosition = (150, newPosition.Item1 - 100);
            newFacing = 3;
        }

        return board[newPosition] == '#' ? (currentPosition, facing) : (newPosition, newFacing);
    }
}
