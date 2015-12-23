<?php
namespace AdventOfCode\Day21\PlayerDecorators\Weapons;

use AdventOfCode\Day21\PlayerDecorator;

class GreataxeDecorator extends PlayerDecorator
{
    public function getDamage()
    {
        return parent::getDamage() + 8;
    }

    public function getCost()
    {
        return parent::getCost() + 74;
    }
}
