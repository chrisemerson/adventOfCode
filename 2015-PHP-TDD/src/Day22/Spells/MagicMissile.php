<?php

namespace AdventOfCode\Day22\Spells;

class MagicMissile extends AbstractSpell
{
    public function getManaCost(): int
    {
        return 53;
    }

    public function getInstantDamage(): int
    {
        return 4;
    }
}