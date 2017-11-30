<?php
namespace AdventOfCode\Day21\PlayerDecorators\DefenceRings;

use AdventOfCode\Day21\PlayerDecorator;

class PlusTwoDefenceDecorator extends PlayerDecorator
{
    public function getArmor()
    {
        return parent::getArmor() + 2;
    }

    public function getCost()
    {
        return parent::getCost() + 40;
    }
}
