<?php
namespace AdventOfCode\Day21\PlayerDecorators\DamageRings;

use AdventOfCode\Day21\PlayerDecorator;

class PlusOneDamageRing extends PlayerDecorator
{
    public function getDamage()
    {
        return parent::getDamage() + 1;
    }

    public function getCost()
    {
        return parent::getCost() + 25;
    }
}
