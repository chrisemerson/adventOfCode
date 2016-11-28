<?php
namespace AdventOfCode\Day21\PlayerDecorators\DefenceRings;

use AdventOfCode\Day21\PlayerDecorator;

class PlusThreeDefenceDecorator extends PlayerDecorator
{
    public function getArmor()
    {
        return parent::getArmor() + 3;
    }

    public function getCost()
    {
        return parent::getCost() + 80;
    }
}
