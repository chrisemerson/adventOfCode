<?php
namespace AdventOfCode\Day14;

class ReindeerRace
{
    private $reindeer = [];

    const REGEX = '/^(.*) can fly (\d+) km\\/s for (\d+) seconds, but then must rest for (\d+) seconds\.$/';

    public function addReindeer($reindeerStats)
    {
        if (preg_match(self::REGEX, $reindeerStats, $matches)) {
            list (, $reindeer, $speed, $duration, $rest) = $matches;

            $this->reindeer[$reindeer] = [
                'speed' => $speed,
                'duration' => $duration,
                'rest' => $rest
            ];
        }
    }

    public function getWinningReindeerDistance($lengthOfRace)
    {
        return max(
            array_map(
                function ($reindeerStats) use ($lengthOfRace) {
                    return $this->getReindeerDistance($reindeerStats, $lengthOfRace);
                },
                $this->reindeer
            )
        );
    }

    public function getWinningReindeerPoints($lengthOfRace)
    {
        $points = array_combine(array_keys($this->reindeer), array_fill(0, count($this->reindeer), 0));

        for ($time = 1; $time <= $lengthOfRace; $time++) {
            $winningDistanceAfterThisTime = $this->getWinningReindeerDistance($time);

            foreach ($this->reindeer as $reindeer => $stats) {
                if ($this->getReindeerDistance($stats, $time) == $winningDistanceAfterThisTime) {
                    $points[$reindeer]++;
                }
            }
        }

        return max($points);
    }

    private function getReindeerDistance($reindeerStats, $lengthOfRace)
    {
        $cycleLength = $reindeerStats['duration'] + $reindeerStats['rest'];
        $excess = $lengthOfRace % $cycleLength;

        return
            //Calculate distance for number of complete cycles
            $reindeerStats['speed'] * $reindeerStats['duration'] * ($lengthOfRace - $excess) / $cycleLength

            //Calculate distance left over after all complete cycles have run
            + $reindeerStats['speed'] * min($excess, $reindeerStats['duration']);
    }
}
