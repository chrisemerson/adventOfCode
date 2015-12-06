<?php
namespace AdventOfCode\Day6;

class ChristmasLights
{
    private $lights = [];

    public function __construct()
    {
        $row = str_repeat('0', 1000);

        foreach (range(0, 999) as $x) {
            $this->lights[$x] = $row;
        }
    }

    public function addInstruction($instruction)
    {
        if (preg_match('/^(?P<instruction>turn (on|off)|toggle) (?P<x1>\d+),(?P<y1>\d+) through (?P<x2>\d+),(?P<y2>\d+)$/', $instruction, $matches)) {
            list($x1, $x2, $y1, $y2) = $this->getNormalisedValuesFromMatches($matches);

            for ($x = $x1; $x <= $x2; $x++) {
                for ($y = $y1; $y <= $y2; $y++) {
                    $this->applyInstructionToLight($matches['instruction'], $x, $y);
                }
            }
        }
    }

    private function getNormalisedValuesFromMatches($matches)
    {
        return [
            min($matches['x1'], $matches['x2']),
            max($matches['x1'], $matches['x2']),
            min($matches['y1'], $matches['y2']),
            max($matches['y1'], $matches['y2'])
        ];
    }

    private function applyInstructionToLight($instruction, $x, $y)
    {
        switch ($instruction) {
            case 'turn on':
                $this->turnOn($x, $y);
                break;

            case 'turn off':
                $this->turnOff($x, $y);
                break;

            case 'toggle':
                $this->toggle($x, $y);
                break;
        }
    }

    public function getNumberOfSwitchedOnLights()
    {
        return array_reduce($this->lights, function($carry, $item) {
            return $carry + array_sum(str_split($item));
        });
    }
    private function turnOn($x, $y)
    {
        $this->lights[$x]{$y} = 1;
    }
    private function turnOff($x, $y)
    {
        $this->lights[$x]{$y} = 0;
    }

    private function toggle($x, $y)
    {
        if ($this->lights[$x]{$y} == 1) {
            $this->turnOff($x, $y);
        } else {
            $this->turnOn($x, $y);
        }
    }
}
