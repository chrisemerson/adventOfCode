<?php
namespace AdventOfCode\Day21\PlayerDecorators\DamageRings;

use AdventOfCode\Day21\PlayerDecorator;

class PlusThreeDamageRing extends PlayerDecorator
{
    public function getDamage()
    {
        return parent::getDamage() + 3;
    }

    public function getCost()
    {
        return parent::getCost() + 100;
    }
}
