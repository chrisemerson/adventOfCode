<?php
namespace AdventOfCode\Day1;

class NotQuiteLisp
{
    private $floor = 0;

    public function getFloor()
    {
        return $this->floor;
    }

    public function move($input)
    {
        $this->floor = substr_count($input, "(") - substr_count($input, ")");
    }
}
