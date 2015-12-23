<?php

namespace AdventOfCode\Day21;

class Game
{
    private $players = [];

    public function addPlayer($label, Player $player)
    {
        $this->players[] = ['label' => $label, 'player' => $player];
    }

    public function playGame()
    {
        $currentPlayer = 0;
        $opponent = 1;

        while (true) {
            $this->players[$opponent]['player']->attack($this->players[$currentPlayer]['player']->getDamage());

            if ($this->players[$opponent]['player']->isDefeated()) {
                return $this->players[$currentPlayer]['label'];
            }

            $currentPlayer = 1 - $currentPlayer;
            $opponent = 1 - $opponent;
        }
    }
}
