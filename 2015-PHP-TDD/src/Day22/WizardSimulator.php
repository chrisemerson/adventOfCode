<?php
namespace AdventOfCode\Day22;

use AdventOfCode\Day22\Spells\Spell;
use Exception;

class WizardSimulator
{
    /** @var Wizard */
    private Wizard $wizard;

    /** @var Boss */
    private Boss $boss;

    /** @var Spell[] */
    private array $spells = [];

    private ?int $minCost = null;

    public function setWizard(Wizard $wizard): void
    {
        $this->wizard = $wizard;
    }

    public function setBoss(Boss $boss): void
    {
        $this->boss = $boss;
    }

    public function addAvailableSpell(Spell $spell): void
    {
        $this->spells[] = $spell;
    }

    public function findCheapestWayForWizardToWinBattle(): int
    {
        return $this->findCostOfCheapestWinToGameRecursive($this->wizard, $this->boss);
    }

    private function findCostOfCheapestWinToGameRecursive(Wizard $wizard, Boss $boss, int $costSoFar = 0): int
    {
        if ($boss->isDefeated()) {
            if (is_null($this->minCost)) {
                $this->minCost = $costSoFar;
            }

            echo $costSoFar . PHP_EOL;

            $this->minCost = min($costSoFar, $this->minCost);

            return 0;
        }

        if ($wizard->isDefeated()) {
            throw new PlayerDefeated();
        }

        if (!is_null($this->minCost) && $costSoFar > $this->minCost) {
            //No point continuing, this branch already costs more than the current best cost
            throw new PlayerDefeated();
        }

        $availableSpells = array_filter(
            $this->spells,
            fn ($s) => $wizard->canReceiveSpell($s) && $boss->canReceiveSpell($s)
        );

        $costsOfCastingSpells = [];

        foreach ($availableSpells as $spell) {
            try {
                [$newWizard, $newBoss] = $this->performTurnsUsingSpell($wizard, $boss, $spell);

                $cost = $spell->getManaCost() + $this->findCostOfCheapestWinToGameRecursive(
                        $newWizard,
                        $newBoss,
                        $costSoFar + $spell->getManaCost()
                    );

                $costsOfCastingSpells[] = $cost;
            } catch (Exception $e)
            {}
        }

        if (count($costsOfCastingSpells) == 0) {
            throw new PlayerDefeated();
        }

        return min($costsOfCastingSpells);
    }

    private function performTurnsUsingSpell(Wizard $wizard, Boss $boss, Spell $spell)
    {
        $newWizard = clone $wizard;
        $newBoss = clone $boss;

        //Wizard's Turn
        $newWizard->performSpellEffects();
        $newBoss->performSpellEffects();

        $newWizard->castSpell($spell);
        $newBoss->castSpell($spell);

        if (!$newBoss->isDefeated()) {
            //Boss's Turn
            $newWizard->performSpellEffects();
            $newBoss->performSpellEffects();

            $newWizard->receiveDamage($newBoss->getAttackPoints());
        }

        return [$newWizard, $newBoss];
    }
}
