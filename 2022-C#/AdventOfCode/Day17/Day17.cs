namespace AdventOfCode;

public class Day17 : IAdventOfCodeDay
{
    private bool[][,] shapes = {
        new[,] {
            { true, false, false, false },
            { true, false, false, false },
            { true, false, false, false },
            { true, false, false, false }
        },
        new[,] {
            { false, true, false, false },
            { true, true, true, false },
            { false, true, false, false },
            { false, false, false, false }
        },
        new[,] {
            { true, false, false, false },
            { true, false, false, false },
            { true, true, true, false },
            { false, false, false, false }
        },
        new[,] {
            { true, true, true, true },
            { false, false, false, false },
            { false, false, false, false },
            { false, false, false, false }
        },
        new[,] {
            { true, true, false, false },
            { true, true, false, false },
            { false, false, false, false },
            { false, false, false, false }
        }
    };

    public void Part1(string input)
    {
        Console.WriteLine("Tower is " + TowerHeightAfterDroppingPieces(input, 2022) + " blocks high");
    }

    public void Part2(string input)
    {
        Console.WriteLine("Tower is " + TowerHeightAfterDroppingPieces(input, 1000000000000) + " blocks high");
    }

    private long TowerHeightAfterDroppingPieces(string input, long piecesToDrop)
    {
        var currentJetPos = 0;
        var grid = new List<(int, long)>();

        const int settlingPeriod = 500;
        var towerHeightsByJetPos = new Dictionary<int, (long, long)>();

        for (long i = 0; i < piecesToDrop; i++) {
            if (i % 5 == 0 && i > settlingPeriod) {
                if (towerHeightsByJetPos.ContainsKey(currentJetPos)) {
                    var heightOfCycle = grid.Select(c => c.Item2).Max() - towerHeightsByJetPos[currentJetPos].Item2;
                    var lengthOfCycle = i - towerHeightsByJetPos[currentJetPos].Item1;
                    var piecesLeft = (piecesToDrop - i) % lengthOfCycle;

                    if (piecesLeft == 0) {
                        var totalHeight = ((piecesToDrop - i) / lengthOfCycle) * heightOfCycle +
                                          grid.Select(c => c.Item2).Max() + 1;
                        return totalHeight;
                    }

                    towerHeightsByJetPos.Remove(currentJetPos);
                }

                towerHeightsByJetPos.Add(currentJetPos, (i, grid.Select(c => c.Item2).Max()));
            }

            if (currentJetPos == 0 && i % 5 == 0 && i > 0) {
                var currentHeight = grid.Select(c => c.Item2).Max();
                var numberOfCycles = (int)Math.Floor((decimal)piecesToDrop / i);

                return currentHeight * numberOfCycles + TowerHeightAfterDroppingPieces(input, piecesToDrop % i);
            }

            var shape = shapes.ElementAt((int)(i % shapes.Length));
            var pieceLocation = (2, (grid.Any() ? grid.Select(c => c.Item2).Max() + 1 : 0) + 3);
            var rockNotSettled = true;

            while (rockNotSettled) {
                //First, move piece according to jet
                var newPieceLocation = input[currentJetPos] == '<'
                    ? (Math.Max(0, pieceLocation.Item1 - 1), pieceLocation.Item2)
                    : (Math.Min(7 - GetShapeDimensions(shape).Item1, pieceLocation.Item1 + 1), pieceLocation.Item2);

                if (CanShapeFitInGrid(grid, shape, newPieceLocation)) {
                    pieceLocation = newPieceLocation;
                }

                currentJetPos = (currentJetPos + 1) % input.Trim().Length;

                //Now try to move down
                newPieceLocation = (pieceLocation.Item1, pieceLocation.Item2 - 1);

                if (CanShapeFitInGrid(grid, shape, newPieceLocation)) {
                    pieceLocation = newPieceLocation;
                } else {
                    rockNotSettled = false;
                }
            }

            //Place shape into grid
            for (var x = 0; x < shape.GetLength(0); x++) {
                for (var y = 0; y < shape.GetLength(1); y++) {
                    if (shape[x, y]) {
                        grid.Add((pieceLocation.Item1 + x, pieceLocation.Item2 + y));
                    }
                }
            }

            //Only bother keeping the top 100 rows of the grid
            grid = grid.Where(c => c.Item2 > grid.Select(r => r.Item2).Max() - 100).ToList();
        }

        return grid.Select(c => c.Item2).Max() + 1;
    }

    private static (int, int) GetShapeDimensions(bool[,] shape)
    {
        var width = 0;
        var height = 0;

        for (var x = 0; x < shape.GetLength(0); x++) {
            for (var y = 0; y < shape.GetLength(1); y++) {
                if (shape[x, y]) {
                    width = x;
                    height = y;
                }
            }
        }

        return (width + 1, height + 1);
    }

    private static bool CanShapeFitInGrid(
        ICollection<(int, long)> grid,
        bool[,] shape,
        (int, long) shapeLocation
    ) {
        for (var x = 0; x < shape.GetLength(0); x++) {
            for (var y = 0; y < shape.GetLength(1); y++) {
                if (shape[x, y] && grid.Contains((shapeLocation.Item1 + x, shapeLocation.Item2 + y))) {
                    return false;
                }
            }
        }

        return shapeLocation.Item2 >= 0;
    }
}
