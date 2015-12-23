<?php
namespace AdventOfCode\Day21\PlayerDecorators\Weapons;

use AdventOfCode\Day21\PlayerDecorator;

class DaggerDecorator extends PlayerDecorator
{
    public function getDamage()
    {
        return parent::getDamage() + 4;
    }

    public function getCost()
    {
        return parent::getCost() + 8;
    }
}
