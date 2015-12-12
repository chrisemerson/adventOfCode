<?php

namespace AdventOfCode\Day12;

class NumberFinder
{
    public function getNumberTotal($inputString)
    {
        $total = 0;

        if (preg_match_all('/-?\d+/', $inputString, $matches)) {
            $total = array_sum($matches[0]);
        }

        return $total;
    }
}
