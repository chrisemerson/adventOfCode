<?php
namespace AdventOfCode\Day21;

class RPGSim
{
    private $decorators = [];
    private $players = [];

    public function __construct(array $decorators = [])
    {
        $this->decorators = $decorators;
    }

    public function addPlayer(string $label, Player $player)
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
        return array_filter(
            $this->getCombinationsOfArrayItems($this->decorators),
            function (array $decorators) {
                return count(array_unique($decorators)) == count($decorators);
            }
        );
    }

    private function getCombinationsOfArrayItems(array $listOfLists)
    {
        $combinations = [];

        if (!empty($listOfLists)) {
            $combinationsOfRemainingItems = $this->getCombinationsOfArrayItems(array_slice($listOfLists, 1));

            foreach ($listOfLists[0] as $item) {
                foreach ($combinationsOfRemainingItems as $combinationOfRemainingItems) {
                    $combinations[] = array_merge($combinationOfRemainingItems, [$item]);
                }
            }
        } else {
            $combinations[] = [];
        }

        return $combinations;
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
