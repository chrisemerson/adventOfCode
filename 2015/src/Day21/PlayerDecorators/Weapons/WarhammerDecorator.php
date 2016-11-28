<?php
namespace AdventOfCode\Day21\PlayerDecorators\Weapons;

use AdventOfCode\Day21\PlayerDecorator;

class WarhammerDecorator extends PlayerDecorator
{
    public function getDamage()
    {
        return parent::getDamage() + 6;
    }

    public function getCost()
    {
        return parent::getCost() + 25;
    }
}
