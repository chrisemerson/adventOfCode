<?php
namespace AdventOfCode\Day21\PlayerDecorators\Weapons;

use AdventOfCode\Day21\PlayerDecorator;

class ShortswordDecorator extends PlayerDecorator
{
    public function getDamage()
    {
        return parent::getDamage() + 5;
    }

    public function getCost()
    {
        return parent::getCost() + 10;
    }
}
