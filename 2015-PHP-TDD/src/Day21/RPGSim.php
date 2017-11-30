<?php
namespace AdventOfCode\Day21;

class RPGSim
{
    private $decorators = [];
    private $players = [];

    public function __construct($decorators = [])
    {
        $this->decorators = $decorators;
    }

    public function addPlayer($label, Player $player)
    {
        $this->players[$label] = $player;
    }

    public function cheapestWin($playerToWin)
    {
        return min(
            array_map(
                function ($combination) use ($playerToWin) {
                    return $this->getCostForCombination($combination, $playerToWin);
                },
                array_filter(
                    $this->getDecoratorCombinations(),
                    function ($combination) use ($playerToWin) {
                        return $this->playerWinsGame($combination, $playerToWin);
                    }
                )
            )
        );
    }

    public function costliestLose($playerToLose)
    {
        return max(
            array_map(
                function ($combination) use ($playerToLose) {
                    return $this->getCostForCombination($combination, $playerToLose);
                },
                array_filter(
                    $this->getDecoratorCombinations(),
                    function ($combination) use ($playerToLose) {
                        return !$this->playerWinsGame($combination, $playerToLose);
                    }
                )
            )
        );
    }

    private function getDecoratorCombinations()
    {
        $decoratorCombinations = [];

        foreach ($this->decorators[0] as $decorator0Class) {
            foreach ($this->decorators[1] as $decorator1Class) {
                foreach ($this->decorators[2] as $decorator2Class) {
                    foreach ($this->decorators[3] as $decorator3Class) {
                        if ($decorator2Class != $decorator3Class) {
                            $decoratorCombinations[] = [$decorator0Class, $decorator1Class, $decorator2Class, $decorator3Class];
                        }
                    }
                }
            }
        }

        return $decoratorCombinations;
    }

    private function playerWinsGame($combination, $playerToWin)
    {
        $player = $this->getDecoratedPlayerFromCombination($combination, $playerToWin);

        $game = new Game();
        $game->addPlayer($playerToWin, $player);

        foreach ($this->players as $label => $thisPlayer) {
            if ($label != $playerToWin) {
                $game->addPlayer($label, clone $thisPlayer);
            }
        }

        return ($game->playGame() == $playerToWin);
    }

    private function getCostForCombination($combination, $playerToWin)
    {
        return $this->getDecoratedPlayerFromCombination($combination, $playerToWin)->getCost();
    }

    private function getDecoratedPlayerFromCombination($combination, $playerToWin)
    {
        return array_reduce(
            $combination,
            function($carry, $item)
            {
                return new $item($carry);
            },
            clone $this->players[$playerToWin]
        );
    }
}
