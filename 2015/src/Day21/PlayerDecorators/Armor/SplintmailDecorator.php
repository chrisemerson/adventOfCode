<?php
namespace AdventOfCode\Day21\PlayerDecorators\Armor;

use AdventOfCode\Day21\PlayerDecorator;

class SplintmailDecorator extends PlayerDecorator
{
    public function getArmor()
    {
        return parent::getArmor() + 3;
    }

    public function getCost()
    {
        return parent::getCost() + 53;
    }
}
