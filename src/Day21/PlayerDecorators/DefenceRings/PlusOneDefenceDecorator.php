<?php
namespace AdventOfCode\Day21\PlayerDecorators\DefenceRings;

use AdventOfCode\Day21\PlayerDecorator;

class PlusOneDefenceDecorator extends PlayerDecorator
{
    public function getArmor()
    {
        return parent::getArmor() + 1;
    }

    public function getCost()
    {
        return parent::getCost() + 20;
    }
}
