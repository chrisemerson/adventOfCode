<?php
namespace AdventOfCode\Day18;

class GameOfLife
{
    private $state = [];
    private $newState = [];

    const LIGHT_ON = "#";
    const LIGHT_OFF = ".";

    public function setInitialState($initialState)
    {
        $lines = explode("\n", $initialState);

        foreach ($lines as $line) {
            $this->state[] = str_split($line, 1);
        }
    }

    public function getNumberOfLightsTurnedOn()
    {
        return array_reduce(
            $this->state,
            function($carry, $item) {
                $counts = array_count_values($item);

                if (isset($counts[self::LIGHT_ON])) {
                    $carry += $counts[self::LIGHT_ON];
                }

                return $carry;
            },
            0
        );
    }

    public function iterate($cornerLightsStuckOn = false)
    {
        $this->initialiseNewState();

        if ($cornerLightsStuckOn) {
            $this->turnOnCornerLights();
        }

        for ($i = 0; $i < count($this->newState); $i++) {
            for ($j = 0; $j < count($this->newState[$i]); $j++) {
                $this->iterateIndividualLight($i, $j);
            }
        }

        if ($cornerLightsStuckOn) {
            $this->turnOnCornerLights();
        }

        $this->commitNewState();
    }

    private function initialiseNewState()
    {
        $this->newState = $this->state;
    }

    private function iterateIndividualLight($i, $j)
    {
        if ($this->getNumberOfNeighbouringLightsOn($i, $j) == 3) {
            $this->turnLightOn($i, $j);
        } elseif ($this->getNumberOfNeighbouringLightsOn($i, $j) != 2) {
            $this->turnLightOff($i, $j);
        }
    }

    private function commitNewState()
    {
        $this->state = $this->newState;
    }

    private function getNumberOfNeighbouringLightsOn($i, $j)
    {
        $neighbours = [];

        for ($a = $i - 1; $a <= $i + 1; $a++) {
            for ($b = $j - 1; $b <= $j + 1; $b++) {
                if (($a != $i || $b != $j) && $this->isValidPosition($a, $b)) {
                    $neighbours[] = $this->state[$a][$b];
                }
            }
        }

        $counts = array_count_values($neighbours);

        return isset($counts[self::LIGHT_ON]) ? $counts[self::LIGHT_ON] : 0;
    }

    private function isValidPosition($i, $j)
    {
        return
            $i >= 0
            && $i < count($this->state)
            && $j >= 0
            && $j < count($this->state[0]);
    }

    private function turnLightOn($i, $j)
    {
        $this->newState[$i][$j] = self::LIGHT_ON;
    }

    private function turnLightOff($i, $j)
    {
        $this->newState[$i][$j] = self::LIGHT_OFF;
    }

    private function turnOnCornerLights()
    {
        $iSize = count($this->state) - 1;
        $jSize = count($this->state[0]) - 1;

        $this->newState[0][0] = self::LIGHT_ON;
        $this->newState[0][$jSize] = self::LIGHT_ON;
        $this->newState[$iSize][0] = self::LIGHT_ON;
        $this->newState[$iSize][$jSize] = self::LIGHT_ON;
    }
}
