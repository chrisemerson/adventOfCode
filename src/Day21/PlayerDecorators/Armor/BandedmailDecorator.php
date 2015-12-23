<?php
namespace AdventOfCode\Day21\PlayerDecorators\Armor;

use AdventOfCode\Day21\PlayerDecorator;

class BandedmailDecorator extends PlayerDecorator
{
    public function getArmor()
    {
        return parent::getArmor() + 4;
    }

    public function getCost()
    {
        return parent::getCost() + 75;
    }
}
