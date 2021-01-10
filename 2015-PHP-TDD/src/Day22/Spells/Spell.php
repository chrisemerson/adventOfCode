<?php

namespace AdventOfCode\Day22\Spells;

interface Spell
{
    public function getManaCost(): int;

    public function getInstantDamage(): int;
    public function getPerTurnDamage(): int;

    public function getInstantPointsHealed(): int;
    public function getPerTurnPointsHealed(): int;

    public function getPerTurnArmor(): int;

    public function getPerTurnManaRecharge(): int;

    public function getSpellLengthInTurns(): int;
}