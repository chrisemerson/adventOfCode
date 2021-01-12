<?php

namespace AdventOfCode\Day22;

use AdventOfCode\Day22\Spells\Spell;

class Wizard extends Player
{
    public function __construct(
        int $hitPoints,
        private int $mana,
        private bool $hardMode
    ) {
        parent::__construct($hitPoints);
    }

    public function castSpell(Spell $spell)
    {
        $this->mana -= $spell->getManaCost();

        if ($this->mana < 0) {
            throw new CantAffordSpell();
        }

        parent::castSpell($spell);
    }

    protected function performInstantSpellEffects(Spell $spell)
    {
        $this->hitPoints += $spell->getInstantPointsHealed();
    }

    protected function performPerTurnSpellEffects(Spell $spell)
    {
        if ($this->hardMode) {
            $this->hitPoints -= 1;

            if ($this->isDefeated()) {
                throw new PlayerDefeated();
            }
        }

        $this->armor += $spell->getPerTurnArmor();
        $this->hitPoints += $spell->getPerTurnPointsHealed();
        $this->mana += $spell->getPerTurnManaRecharge();
    }

    public function isHardMode(): bool
    {
        return $this->hardMode;
    }
}
