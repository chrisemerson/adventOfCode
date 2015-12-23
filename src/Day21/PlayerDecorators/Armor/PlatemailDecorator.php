<?php
namespace AdventOfCode\Day21\PlayerDecorators\Armor;

use AdventOfCode\Day21\PlayerDecorator;

class PlatemailDecorator extends PlayerDecorator
{
    public function getArmor()
    {
        return parent::getArmor() + 5;
    }

    public function getCost()
    {
        return parent::getCost() + 102;
    }
}
