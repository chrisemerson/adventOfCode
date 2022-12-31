namespace AdventOfCode;

struct CurrentState
{
    public (string, int) CurrentPosition;
    public (string, int)? ElephantPosition;
    public int TimeLeft;
    public Dictionary<string, Valve> Valves;

    public CurrentState(
        (string, int) currentPosition,
        int timeLeft,
        Dictionary<string, Valve> valves,
        (string, int)? elephantPosition = null
    ) {
        CurrentPosition = currentPosition;
        ElephantPosition = elephantPosition;
        TimeLeft = timeLeft;
        Valves = valves;
    }
}
