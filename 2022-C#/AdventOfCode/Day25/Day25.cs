namespace AdventOfCode;

public class Day25 : IAdventOfCodeDay
{
    public void Part1(string input) => Console.WriteLine(
        "SNAFU No: " +
        DecimalToSNAFU(
            input
                .Split("\n",
                    StringSplitOptions.TrimEntries |
                    StringSplitOptions.RemoveEmptyEntries)
                .Select(SNAFUToDecimal)
                .Sum()));

    public void Part2(string input) => throw new NotImplementedException();


    private string DecimalToSNAFU(long dec)
    {
        //First, convert to base 5
        var powersOf5 = new Dictionary<int, long> { { 0, 1 } };
        var power = 0;
        var baseFive = "";

        while (5 * Math.Pow(5, power) < dec) {
            power++;
            powersOf5.Add(power, (long) Math.Pow(5, power));
        }

        foreach (var po5 in powersOf5.Reverse()) {
            int i;
            for (i = 5; po5.Value * i > dec; i--);
            baseFive += i;
            dec -= po5.Value * i;
        }

        //Then convert to SNAFU
        var returnString = "";
        var carry = 0;

        foreach (var ch in baseFive.Reverse()) {
            var val = int.Parse(ch.ToString()) + carry;

            if (val > 2) {
                returnString += val == 4 ? "-" : "=";
                carry = 1;
            } else {
                returnString += val;
                carry = 0;
            }
        }

        if (carry == 1) {
            returnString += "1";
        }

        return returnString.Reverse().Aggregate("", (acc, c) => acc + c);
    }

    private long SNAFUToDecimal(string snafu)
    {
        var result = (long)0;

        for (var i = 1; i <= snafu.Length; i++) {
            result += snafu[i - 1] switch {
                '=' => -2,
                '-' => -1,
                '0' => 0,
                '1' => 1,
                '2' => 2,
                _ => 1
            } * (long)Math.Pow(5, snafu.Length - i);
        }

        return result;
    }
}
