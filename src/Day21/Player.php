<?php
namespace AdventOfCode\Day21;

interface Player
{
    public function isDefeated();
    public function attack($damage);
    public function getDamage();
    public function getArmor();
    public function getCost();
    public function getHitPoints();
}
