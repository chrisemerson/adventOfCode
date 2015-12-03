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
            if ($this->isMoveUpCharacter($input[$i])) {
                $this->floor++;
            }

            if ($this->isMoveDownCharacter($input[$i])) {
                $this->floor--;
            }

            if (!$this->haveVisitedBasement() && $this->justEnteredBasement()) {
                $this->firstBasementMove = $i + 1;
            }
        }
    }

    public function getFirstBasementMove()
    {
        return $this->firstBasementMove;
    }

    private function isMoveUpCharacter($charAtThisPosition)
    {
        return $charAtThisPosition == self::MOVE_UP_CHAR;
    }

    private function isMoveDownCharacter($charAtThisPosition)
    {
        return $charAtThisPosition == self::MOVE_DOWN_CHAR;
    }

    private function justEnteredBasement()
    {
        return $this->floor == -1;
    }

    private function haveVisitedBasement()
    {
        return $this->firstBasementMove != 0;
    }
}
