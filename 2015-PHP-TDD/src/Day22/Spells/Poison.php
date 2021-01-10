<?php

namespace AdventOfCode\Day22\Spells;

class Poison extends AbstractSpell
{
    public function getManaCost(): int
    {
        return 173;
    }

    public function getPerTurnDamage(): int
    {
        return 3;
    }

    public function getSpellLengthInTurns(): int
    {
        return 6;
    }
}