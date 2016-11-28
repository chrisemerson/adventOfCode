<?php
namespace AdventOfCode\Day3;

class HouseVisitor
{
    /** @var Visitor[] */
    private $visitors;

    /** @var Visitor */
    private $currentVisitor;

    private $combinedVisitedHouses = [];

    const MOVE_LEFT = '<';
    const MOVE_RIGHT = '>';
    const MOVE_UP = '^';
    const MOVE_DOWN = 'v';

    public function __construct(array $visitors)
    {
        $this->visitors = $visitors;

        $this->nextVisitor();
    }

    public function getNumberOfHousesVisited()
    {
        foreach ($this->visitors as $visitor) {
            $visitedHouses = $visitor->getHousesVisited();

            foreach ($visitedHouses as $x => $yValues) {
                foreach (array_keys($yValues) as $y) {
                    $this->visitHouseAt($x, $y);
                }
            }
        }

        return array_reduce(
            $this->combinedVisitedHouses,
            function ($carry, $item) {
                return $carry + count($item);
            }
        );
    }

    public function visitHouses($directions)
    {
        for ($i = 0; $i < strlen($directions); $i++) {
            $this->processDirection($directions[$i]);
            $this->nextVisitor();
        }
    }

    private function processDirection($currentDirection)
    {
        switch (strtolower($currentDirection)) {
            case self::MOVE_LEFT:
                $this->currentVisitor->moveLeft();
                break;

            case self::MOVE_RIGHT:
                $this->currentVisitor->moveRight();
                break;

            case self::MOVE_UP:
                $this->currentVisitor->moveUp();
                break;

            case self::MOVE_DOWN:
                $this->currentVisitor->moveDown();
                break;
        }
    }

    private function nextVisitor()
    {
        $this->currentVisitor = array_shift($this->visitors);
        $this->visitors[] = $this->currentVisitor;
    }

    private function visitHouseAt($x, $y)
    {
        if (!isset($this->combinedVisitedHouses[$x])) {
            $this->combinedVisitedHouses[$x] = [$y => true];
        } else {
            $this->combinedVisitedHouses[$x][$y] = true;
        }
    }
}
