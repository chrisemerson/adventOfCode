<?php
namespace AdventOfCode\Day21\PlayerDecorators\DamageRings;

use AdventOfCode\Day21\PlayerDecorator;

class PlusTwoDamageRing extends PlayerDecorator
{
    public function getDamage()
    {
        return parent::getDamage() + 2;
    }

    public function getCost()
    {
        return parent::getCost() + 50;
    }
}
