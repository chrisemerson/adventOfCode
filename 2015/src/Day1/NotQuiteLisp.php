<?php
namespace AdventOfCode\Day1;

class NotQuiteLisp
{
    private $floor = 0;
    private $firstBasementMove = 0;

    const MOVE_UP_CHAR = "(";
    const MOVE_DOWN_CHAR = ")";

    public function getFloor()
    {
        return $this->floor;
    }

    public function move($input)
    {
        for ($i = 0; $i < strlen($input); $i++) {
            $this->processMove($input[$i]);
            $this->checkForAndProcessBasementVisit($i);
        }
    }

    public function getFirstBasementMove()
    {
        return $this->firstBasementMove;
    }

    private function processMove($move)
    {
        if ($this->isMoveUpCharacter($move)) {
            $this->goUpOneFloor();
        }

        if ($this->isMoveDownCharacter($move)) {
            $this->goDownOneFloor();
        }
    }

    private function isMoveUpCharacter($charAtThisPosition)
    {
        return $charAtThisPosition == self::MOVE_UP_CHAR;
    }

    private function goUpOneFloor()
    {
        $this->floor++;
    }

    private function isMoveDownCharacter($charAtThisPosition)
    {
        return $charAtThisPosition == self::MOVE_DOWN_CHAR;
    }

    private function goDownOneFloor()
    {
        $this->floor--;
    }

    private function checkForAndProcessBasementVisit($i)
    {
        if ($this->inBasementForFirstTime()) {
            $this->logFirstBasementMove($i + 1);
        }
    }

    private function inBasementForFirstTime()
    {
        return !$this->haveVisitedBasement() && $this->atFirstBasementFloor();
    }

    private function atFirstBasementFloor()
    {
        return $this->getFloor() == -1;
    }

    private function haveVisitedBasement()
    {
        return $this->firstBasementMove != 0;
    }

    private function logFirstBasementMove($character)
    {
        return $this->firstBasementMove = $character;
    }
}
