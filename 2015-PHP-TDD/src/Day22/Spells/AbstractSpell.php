<?php

namespace AdventOfCode\Day22\Spells;

abstract class AbstractSpell implements Spell
{
    public function getInstantDamage(): int
    {
        return 0;
    }

    public function getPerTurnDamage(): int
    {
        return 0;
    }

    public function getInstantPointsHealed(): int
    {
        return 0;
    }

    public function getPerTurnPointsHealed(): int
    {
        return 0;
    }

    public function getPerTurnArmor(): int
    {
        return 0;
    }

    public function getPerTurnManaRecharge(): int
    {
        return 0;
    }

    public function getSpellLengthInTurns(): int
    {
        return 0;
    }
}