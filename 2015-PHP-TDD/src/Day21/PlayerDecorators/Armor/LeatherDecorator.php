<?php
namespace AdventOfCode\Day21\PlayerDecorators\Armor;

use AdventOfCode\Day21\PlayerDecorator;

class LeatherDecorator extends PlayerDecorator
{
    public function getArmor()
    {
        return parent::getArmor() + 1;
    }

    public function getCost()
    {
        return parent::getCost() + 13;
    }
}
