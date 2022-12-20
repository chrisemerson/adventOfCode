namespace AdventOfCode;

struct CurrentState
{
    public string CurrentPosition;
    public int TimeLeft;
    public Dictionary<string, Valve> Valves;

    public CurrentState(string currentPosition, int timeLeft, Dictionary<string, Valve> valves)
    {
        CurrentPosition = currentPosition;
        TimeLeft = timeLeft;
        Valves = valves;
    }
}
