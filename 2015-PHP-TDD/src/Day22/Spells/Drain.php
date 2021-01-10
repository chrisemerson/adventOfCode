<?php

namespace AdventOfCode\Day22\Spells;

class Drain extends AbstractSpell
{
    public function getManaCost(): int
    {
        return 73;
    }

    public function getInstantDamage(): int
    {
        return 2;
    }

    public function getInstantPointsHealed(): int
    {
        return 2;
    }
}