<?php
namespace AdventOfCode\Day3;

class HouseVisitor
{
    private $x = 0;
    private $y = 0;

    private $visitedHouses = [];

    const MOVE_LEFT = '<';
    const MOVE_RIGHT = '>';
    const MOVE_UP = '^';
    const MOVE_DOWN = 'v';

    public function __construct()
    {
        $this->visitHouseAtCurrentLocation();
    }

    public function getNumberOfHousesVisited()
    {
        return array_reduce(
            $this->visitedHouses,
            function ($carry, $item) {
                return $carry + count($item);
            }
        );
    }

    public function visitHouses($directions)
    {
        for ($i = 0; $i < strlen($directions); $i++) {
            $this->processDirection($directions[$i]);
        }
    }

    private function processDirection($currentDirection)
    {
        switch (strtolower($currentDirection)) {
            case self::MOVE_LEFT:
                $this->moveLeft();
                break;

            case self::MOVE_RIGHT:
                $this->moveRight();
                break;

            case self::MOVE_UP:
                $this->moveUp();
                break;

            case self::MOVE_DOWN:
                $this->moveDown();
                break;
        }

        $this->visitHouseAtCurrentLocation();
    }

    private function visitHouseAtCurrentLocation()
    {
        if (!isset($this->visitedHouses[$this->x])) {
            $this->visitedHouses[$this->x] = [$this->y => true];
        } else {
            $this->visitedHouses[$this->x][$this->y] = true;
        }
    }

    private function moveLeft()
    {
        $this->x--;
    }

    private function moveRight()
    {
        $this->x++;
    }

    private function moveUp()
    {
        $this->y++;
    }

    private function moveDown()
    {
        $this->y--;
    }
}
