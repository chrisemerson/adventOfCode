<?php

namespace AdventOfCode\Day25;

class WeatherMachine
{
    public function getNumberAt(int $row, int $col): int
    {
        $sequenceNo = $this->getSequenceNumberAt($row, $col);

        $code = 20151125;

        for ($i = 1; $i < $sequenceNo; $i++) {
            $code = ($code * 252533) % 33554393;
        }

        return $code;
    }

    public function getSequenceNumberAt(int $row, int $col): int
    {
        return (($col + $row - 1) * ($col + $row) / 2) - ($row - 1);
    }
}
