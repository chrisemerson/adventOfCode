<?php
namespace AdventOfCode\Day21;

abstract class PlayerDecorator implements Player
{
    /** @var Player */
    protected $player;

    public function __construct(Player $playerToBeDecorated)
    {
        $this->player = $playerToBeDecorated;
    }

    public function isDefeated()
    {
        return $this->player->isDefeated();
    }

    public function attack($damage)
    {
        return $this->player->attack($damage);
    }

    public function getDamage()
    {
        return $this->player->getDamage();
    }

    public function getArmor()
    {
        return $this->player->getArmor();
    }

    public function getCost()
    {
        return $this->player->getCost();
    }

    public function getHitPoints()
    {
        return $this->player->getHitPoints();
    }
}
