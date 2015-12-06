<?php
namespace AdventOfCode\Day6;

class ChristmasLights
{
    private $lights = [];
    private $brightLights = [];

    public function __construct()
    {
        for ($i = 0; $i <= 999; $i++) {
            $this->lights[$i] = str_repeat(chr(0), 1000);
            $this->brightLights[$i] = str_repeat(chr(0), 1000);
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
                $this->changeBrightness($x, $y, 1);
                break;

            case 'turn off':
                $this->turnOff($x, $y);
                $this->changeBrightness($x, $y, -1);
                break;

            case 'toggle':
                $this->toggle($x, $y);
                $this->changeBrightness($x, $y, 2);
                break;
        }
    }

    public function getNumberOfSwitchedOnLights()
    {
        return array_reduce(
            $this->lights,
            function($carry, $item) {
                return $carry + array_sum(array_map('ord', str_split($item)));
            },
            0
        );
    }

    public function getTotalBrightnessOfLights()
    {
        return array_reduce(
            $this->brightLights,
            function($carry, $item) {
                return $carry + array_sum(array_map('ord', str_split($item)));
            },
            0
        );
    }

    private function turnOn($x, $y)
    {
        $this->lights[$x]{$y} = chr(1);
    }

    private function turnOff($x, $y)
    {
        $this->lights[$x]{$y} = chr(0);
    }

    private function toggle($x, $y)
    {
        $this->lights[$x]{$y} = chr(1 - ord($this->lights[$x]{$y}));
    }

    private function changeBrightness($x, $y, $change)
    {
        $currentBrightness = ord($this->brightLights[$x]{$y});
        $newBrightness = max(0, $currentBrightness + $change);
        $this->brightLights[$x]{$y} = chr($newBrightness);
    }
}
