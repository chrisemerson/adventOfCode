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
        $this->hitPoints -= $damage;
    }

    public function getDamage()
    {
        return $this->damage;
    }

    public function getArmor()
    {
        return $this->armor;
    }

    public function getCost()
    {
        return 0;
    }

    public function getHitPoints()
    {
        return $this->hitPoints;
    }
}
