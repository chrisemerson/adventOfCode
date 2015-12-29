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
            $damageDone = max(1, $this->players[$currentPlayer]['player']->getDamage() - $this->players[$opponent]['player']->getArmor());
            $this->players[$opponent]['player']->attack($damageDone);

            if ($this->players[$opponent]['player']->isDefeated()) {
                return $this->players[$currentPlayer]['label'];
            }

            $currentPlayer = 1 - $currentPlayer;
            $opponent = 1 - $opponent;
        }
    }
}
