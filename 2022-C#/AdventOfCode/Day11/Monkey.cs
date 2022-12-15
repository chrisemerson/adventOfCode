namespace AdventOfCode;

public class Monkey
{
    public readonly int Id;
    private Operator Operator;
    private ulong? Operand1;
    private ulong? Operand2;
    public ulong DivisorForTest;
    private int MonkeyIfTrue;
    private int MonkeyIfFalse;
    private List<ulong> Items;
    private ulong InspectionCount;

    public Monkey(
        int id,
        Operator op,
        ulong? operand1,
        ulong? operand2,
        ulong divisorForTest,
        int monkeyIfTrue,
        int monkeyIfFalse,
        List<ulong> items
    ) {
        Id = id;
        Operator = op;
        Operand1 = operand1;
        Operand2 = operand2;
        DivisorForTest = divisorForTest;
        MonkeyIfTrue = monkeyIfTrue;
        MonkeyIfFalse = monkeyIfFalse;
        Items = items;
    }

    public bool HasItems() => Items.Count > 0;

    public (int, ulong) InspectItem()
    {
        var item = Items.First();
        Items = Items.Skip(1).ToList();

        item = Operator switch {
            Operator.Add => (ulong)Math.Floor(((Operand1 ?? item) + (Operand2 ?? item)) / 3.0),
            Operator.Multiply => (ulong)Math.Floor(((Operand1 ?? item) * (Operand2 ?? item)) / 3.0),
            _ => item
        };

        InspectionCount += 1;

        return item % DivisorForTest == 0
            ? (MonkeyIfTrue, item)
            : (MonkeyIfFalse, item);
    }

    public (int, ulong) InspectItemHarder(ulong moduloToUse)
    {
        var item = Items.First();

        Items = Items.Skip(1).ToList();

        item = Operator switch {
            Operator.Add => (Operand1 ?? item) + (Operand2 ?? item),
            Operator.Multiply => (Operand1 ?? item) * (Operand2 ?? item),
            _ => item
        };

        InspectionCount += 1;

        return item % DivisorForTest == 0
            ? (MonkeyIfTrue, item % moduloToUse)
            : (MonkeyIfFalse, item % moduloToUse);
    }

    public void AcceptItem(ulong itemWorryLevel) => Items.Add(itemWorryLevel);

    public ulong GetInspectionCount() => InspectionCount;
}
