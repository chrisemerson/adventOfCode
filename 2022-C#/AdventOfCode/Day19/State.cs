namespace AdventOfCode;

class State
{
    private Blueprint blueprint;

    public int MinutesLeft;

    public int Ore;
    private int Clay;
    private int Obsidian;
    public int Geodes;

    private int OreRobots = 1;
    private int ClayRobots;
    private int ObsidianRobots;
    private int GeodeRobots;

    private int OreRobotsUnderConstruction;
    private int ClayRobotsUnderConstruction;
    private int ObsidianRobotsUnderConstruction;
    private int GeodeRobotsUnderConstruction;

    public State(Blueprint blueprint)
    {
        this.blueprint = blueprint;
    }

    public State AdvanceTime()
    {
        var newState = CloneThis();

        newState.OreRobots += OreRobotsUnderConstruction;
        newState.ClayRobots += ClayRobotsUnderConstruction;
        newState.ObsidianRobots += ObsidianRobotsUnderConstruction;
        newState.GeodeRobots += GeodeRobotsUnderConstruction;

        newState.OreRobotsUnderConstruction = 0;
        newState.ClayRobotsUnderConstruction = 0;
        newState.ObsidianRobotsUnderConstruction = 0;
        newState.GeodeRobotsUnderConstruction = 0;

        newState.Ore += OreRobots;
        newState.Clay += ClayRobots;
        newState.Obsidian += ObsidianRobots;
        newState.Geodes += GeodeRobots;

        newState.MinutesLeft -= 1;

        return newState;
    }

    private State ConstructOreRobot()
    {
        if (Ore < blueprint.OreRobot.Ore || Clay < blueprint.OreRobot.Clay || Obsidian < blueprint.OreRobot.Obsidian) {
            throw new Exception("Not enough resources");
        }

        var newState = CloneThis();

        newState.Ore -= blueprint.OreRobot.Ore;
        newState.Clay -= blueprint.OreRobot.Clay;
        newState.Obsidian -= blueprint.OreRobot.Obsidian;

        newState.OreRobotsUnderConstruction += 1;

        return newState;
    }

    private State ConstructClayRobot()
    {
        if (
            Ore < blueprint.ClayRobot.Ore
            || Clay < blueprint.ClayRobot.Clay
            || Obsidian < blueprint.ClayRobot.Obsidian
        ) {
            throw new Exception("Not enough resources");
        }

        var newState = CloneThis();

        newState.Ore -= blueprint.ClayRobot.Ore;
        newState.Clay -= blueprint.ClayRobot.Clay;
        newState.Obsidian -= blueprint.ClayRobot.Obsidian;

        newState.ClayRobotsUnderConstruction += 1;

        return newState;
    }

    private State ConstructObsidianRobot()
    {
        if (
            Ore < blueprint.ObsidianRobot.Ore
            || Clay < blueprint.ObsidianRobot.Clay
            || Obsidian < blueprint.ObsidianRobot.Obsidian
        ) {
            throw new Exception("Not enough resources");
        }

        var newState = CloneThis();

        newState.Ore -= blueprint.ObsidianRobot.Ore;
        newState.Clay -= blueprint.ObsidianRobot.Clay;
        newState.Obsidian -= blueprint.ObsidianRobot.Obsidian;

        newState.ObsidianRobotsUnderConstruction += 1;

        return newState;
    }

    private State ConstructGeodeRobot()
    {
        if (
            Ore < blueprint.GeodeRobot.Ore
            || Clay < blueprint.GeodeRobot.Clay
            || Obsidian < blueprint.GeodeRobot.Obsidian
        ) {
            throw new Exception("Not enough resources");
        }

        var newState = CloneThis();

        newState.Ore -= blueprint.GeodeRobot.Ore;
        newState.Clay -= blueprint.GeodeRobot.Clay;
        newState.Obsidian -= blueprint.GeodeRobot.Obsidian;

        newState.GeodeRobotsUnderConstruction += 1;

        return newState;
    }

    private State CloneThis()
    {
        return new State(blueprint) {
            MinutesLeft = MinutesLeft,
            Ore = Ore,
            Clay = Clay,
            Obsidian = Obsidian,
            Geodes = Geodes,
            OreRobots = OreRobots,
            ClayRobots = ClayRobots,
            ObsidianRobots = ObsidianRobots,
            GeodeRobots = GeodeRobots,
            OreRobotsUnderConstruction = OreRobotsUnderConstruction,
            ClayRobotsUnderConstruction = ClayRobotsUnderConstruction,
            ObsidianRobotsUnderConstruction = ObsidianRobotsUnderConstruction,
            GeodeRobotsUnderConstruction = GeodeRobotsUnderConstruction
        };
    }

    public List<State> GetOptions()
    {
        var options = new List<State>();

        TryAddOption(options, ConstructGeodeRobot);

        if (options.Count == 1) {
            return options;
        }

        //Do we need any more obsidian robots?
        if (ObsidianRobots < new List<int> {
                blueprint.OreRobot.Obsidian,
                blueprint.ClayRobot.Obsidian,
                blueprint.ObsidianRobot.Obsidian,
                blueprint.GeodeRobot.Obsidian
            }.Max()) {
            TryAddOption(options, ConstructObsidianRobot);
        }

        if (options.Count == 1) {
            return options;
        }

        //Do we need any more clay robots?
        if (ClayRobots < new List<int> {
                blueprint.OreRobot.Clay,
                blueprint.ClayRobot.Clay,
                blueprint.ObsidianRobot.Clay,
                blueprint.GeodeRobot.Clay
            }.Max()) {
            TryAddOption(options, ConstructClayRobot);
        }

        //Do we need any more ore robots?
        if (OreRobots < new List<int> {
                blueprint.OreRobot.Ore,
                blueprint.ClayRobot.Ore,
                blueprint.ObsidianRobot.Ore,
                blueprint.GeodeRobot.Ore
            }.Max()) {
            TryAddOption(options, ConstructOreRobot);
        }

        options.Add(this);

        return options.ToList();
    }

    private static void TryAddOption(List<State> options, Func<State> optionFunction)
    {
        try {
            options.Add(optionFunction());
        } catch (Exception) {
            // ignored
        }
    }

    public bool IsAchievable(int requiredGeodes) => (requiredGeodes - Geodes) / MinutesLeft - GeodeRobots <= 1;
}
