<?php
namespace AdventOfCode\Day21\PlayerDecorators\Weapons;

use AdventOfCode\Day21\PlayerDecorator;

class LongswordDecorator extends PlayerDecorator
{
    public function getDamage()
    {
        return parent::getDamage() + 7;
    }

    public function getCost()
    {
        return parent::getCost() + 40;
    }
}
