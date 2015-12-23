<?php
namespace AdventOfCode\Day21;

class SimplePlayer implements Player
{
    private $hitPoints;
    private $damage;
    private $armor;

    public function __construct($hitPoints, $damage, $armor)
    {
        $this->hitPoints = $hitPoints;
        $this->damage = $damage;
        $this->armor = $armor;
    }

    public function isDefeated()
    {
        return $this->hitPoints <= 0;
    }

    public function attack($damage)
    {
        $damageThisAttack = max(1, $damage - $this->getArmor());

        $this->hitPoints -= $damageThisAttack;
    }

    public function getDamage()
    {
        return $this->damage;
    }

    public function getArmor()
    {
        return $this->armor;
    }
}
