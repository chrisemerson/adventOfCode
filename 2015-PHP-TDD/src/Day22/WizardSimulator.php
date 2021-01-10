<?php
namespace AdventOfCode\Day22;

use AdventOfCode\Day22\Spells\Spell;
use Exception;

class WizardSimulator
{
    /** @var Wizard */
    private $wizard;

    /** @var Boss */
    private $boss;

    /** @var Spell[] */
    private $spells = [];

    public function addWizard(Wizard $wizard): void
    {
        $this->wizard = $wizard;
    }

    public function addBoss(Boss $boss): void
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

    public function findMostExpensiveWayForWizardToLoseBattle(): int
    {
        return $this->findCostOfMostExpensiveLossToGameRecursive($this->wizard, $this->boss);
    }

    private function findCostOfCheapestWinToGameRecursive(Wizard $wizard, Boss $boss, int $costSoFar = 0, ?int $minCostSoFar = null): int
    {
        if ($boss->isDefeated()) {
            return 0;
        }

        if ($wizard->isDefeated()) {
            throw new PlayerDefeated();
        }

        $availableSpells = array_filter(
            $this->spells,
            function ($spell) {
                return $this->wizard->canReceiveSpell($spell) && $this->boss->canReceiveSpell($spell);
            }
        );

        $costsOfCastingSpells = [];

        foreach ($availableSpells as $spell) {
            try {
                $cost = $spell->getManaCost()
                    + $this->findCostOfCheapestWinToGameRecursive(
                        ...$this->performTurnsUsingSpell($wizard, $boss, $spell)
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

    private function findCostOfMostExpensiveLossToGameRecursive(Wizard $wizard, Boss $boss): int
    {
        if ($boss->isDefeated()) {
            throw new PlayerDefeated();
        }

        if ($wizard->isDefeated()) {
            return 0;
        }

        $availableSpells = array_filter(
            $this->spells,
            function ($spell) {
                return $this->wizard->canReceiveSpell($spell) && $this->boss->canReceiveSpell($spell);
            }
        );

        $costsOfCastingSpells = array_map(
            function ($spell) use ($wizard, $boss) {
                try {
                    return
                        $spell->getManaCost()
                        + $this->findCostOfMostExpensiveLossToGameRecursive(
                            ...$this->performTurnsUsingSpell($wizard, $boss, $spell)
                        );
                } catch (PlayerDefeated $e) {
                    return null;
                } catch (CantAffordSpell $e) {
                    return 0;
                }
            },
            $availableSpells
        );

        $availableSpellCosts = array_filter(
            $costsOfCastingSpells,
            function ($cost) {
                return $cost !== null;
            }
        );

        if (count($availableSpellCosts) == 0) {
            return 0;
        }

        return max($availableSpellCosts);
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

        //Boss's Turn
        $newWizard->performSpellEffects();
        $newBoss->performSpellEffects();

        $newWizard->receiveDamage($newBoss->getAttackPoints());

        return [$newWizard, $newBoss];
    }
}
