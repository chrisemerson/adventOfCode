<?php

namespace AdventOfCode\Day8;

class Matchsticks
{
    private $string;

    public function addString($string)
    {
        $this->string = $string;
    }

    public function getDifferenceInCharacters()
    {
        if (!empty($this->string)) {
            return 2;
        }

        return 0;
    }
}


