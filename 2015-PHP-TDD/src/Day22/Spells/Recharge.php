<?php

namespace AdventOfCode\Day22\Spells;

class Recharge extends AbstractSpell
{
    public function getManaCost(): int
    {
        return 229;
    }

    public function getPerTurnManaRecharge(): int
    {
        return 101;
    }

    public function getSpellLengthInTurns(): int
    {
        return 5;
    }
}