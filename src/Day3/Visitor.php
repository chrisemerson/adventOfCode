<?php

namespace AdventOfCode\Day3;

class Visitor
{
    private $x = 0;
    private $y = 0;

    private $visitedHouses = [];

    public function __construct()
    {
        $this->visitHouse();
    }

    public function getHousesVisited()
    {
        return $this->visitedHouses;
    }

    public function moveLeft()
    {
        $this->x--;
        $this->visitHouse();
    }

    public function moveRight()
    {
        $this->x++;
        $this->visitHouse();
    }

    public function moveUp()
    {
        $this->y++;
        $this->visitHouse();
    }

    public function moveDown()
    {
        $this->y--;
        $this->visitHouse();
    }

    private function visitHouse()
    {
        if (!isset($this->visitedHouses[$this->x])) {
            $this->visitedHouses[$this->x] = [$this->y => true];
        } else {
            $this->visitedHouses[$this->x][$this->y] = true;
        }
    }
}
