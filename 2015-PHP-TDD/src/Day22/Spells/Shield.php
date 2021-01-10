<?php

namespace AdventOfCode\Day22\Spells;

class Shield extends AbstractSpell
{
    public function getManaCost(): int
    {
        return 113;
    }

    public function getPerTurnArmor(): int
    {
        return 7;
    }

    public function getSpellLengthInTurns(): int
    {
        return 6;
    }
}