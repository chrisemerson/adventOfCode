<?php
namespace AdventOfCode\Day4;

class StockingStuffer
{
    private $inputString;

    public function findSuffixNumberFor($string, $numberOfZeros = 5)
    {
        $this->inputString = $string;

        for ($i = 1; !$this->hashForNumberSuffixStartsWithZeros($i, $numberOfZeros); $i++);

        return $i;
    }

    private function hashForNumberSuffixStartsWithZeros($numberSuffix, $numberOfZeros)
    {
        return strcmp(substr(md5($this->inputString . $numberSuffix), 0, $numberOfZeros), str_repeat('0', $numberOfZeros)) == 0;
    }
}
