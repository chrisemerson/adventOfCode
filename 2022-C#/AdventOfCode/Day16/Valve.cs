namespace AdventOfCode;

struct Valve
{
    public string Name;
    public bool Open = false;
    public Dictionary<string, int> Connections;
    public int FlowRate;

    public Valve(string name, Dictionary<string, int> connections, int flowRate, bool open = false)
    {
        Name = name;
        Open = open;
        Connections = connections;
        FlowRate = flowRate;
    }
}
