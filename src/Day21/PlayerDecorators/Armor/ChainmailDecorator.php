<?php
namespace AdventOfCode\Day21\PlayerDecorators\Armor;

use AdventOfCode\Day21\PlayerDecorator;

class ChainmailDecorator extends PlayerDecorator
{
    public function getArmor()
    {
        return parent::getArmor() + 2;
    }

    public function getCost()
    {
        return parent::getCost() + 31;
    }
}
