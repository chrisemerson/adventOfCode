<?php

namespace AdventOfCode\Day22;

use AdventOfCode\Day22\Spells\Spell;

class Boss extends Player
{
    public function __construct(
        int $hitPoints,
        private int $attack
    ) {
        parent::__construct($hitPoints);
    }

    public function getAttackPoints(): int
    {
        return $this->attack;
    }

    protected function performInstantSpellEffects(Spell $spell)
    {
        $this->hitPoints -= $spell->getInstantDamage();
    }

    protected function performPerTurnSpellEffects(Spell $spell)
    {
        $this->hitPoints -= $spell->getPerTurnDamage();
    }
}
