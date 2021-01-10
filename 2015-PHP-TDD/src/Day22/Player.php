<?php

namespace AdventOfCode\Day22;

use AdventOfCode\Day22\Spells\Spell;

abstract class Player
{
    private array $activeSpells = [];
    protected int $armor = 0;

    public function __construct(
        protected int $hitPoints
    ) {}

    public function canReceiveSpell(Spell $spell): bool
    {
        foreach ($this->activeSpells as $activeSpell) {
            if ($activeSpell['spell']::class == $spell::class) {
                return $activeSpell['timer'] == 1;
            }
        }

        return true;
    }

    public function castSpell(Spell $spell)
    {
        $this->performInstantSpellEffects($spell);

        if ($spell->getSpellLengthInTurns() > 0) {
            $this->activeSpells[] = [
                'timer' => $spell->getSpellLengthInTurns(),
                'spell' => $spell
            ];
        }
    }

    public function performSpellEffects()
    {
        $newSpells = [];

        $this->armor = 0;

        foreach ($this->activeSpells as $activeSpell) {
            $this->performPerTurnSpellEffects($activeSpell['spell']);

            $newSpells[] = [
                'timer' => $activeSpell['timer'] - 1,
                'spell' => $activeSpell['spell']
            ];
        }

        $this->activeSpells = array_filter($newSpells, function ($activeSpell) {
            return $activeSpell['timer'] > 0;
        });
    }

    public function receiveDamage(int $damageAmount)
    {
        $this->hitPoints -= max(1, $damageAmount - $this->armor);
    }

    public function isDefeated(): bool
    {
        return $this->hitPoints <= 0;
    }

    abstract protected function performInstantSpellEffects(Spell $spell);
    abstract protected function performPerTurnSpellEffects(Spell $spell);
}
