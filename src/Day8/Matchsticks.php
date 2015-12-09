<?php

namespace AdventOfCode\Day8;

class Matchsticks
{
    private $strings;

    public function addString($string)
    {
        $this->strings[] = $string;
    }

    public function getDifferenceInCharacters()
    {
        $difference = 0;

        foreach ($this->strings as $string) {
            $parsedString = $this->parseString($string);

            $difference += (strlen($string) - strlen($parsedString));
        }

        return $difference;
    }

    private function parseString($string)
    {
        $parsedString = str_replace(['\\\\', '\\"'], ['\\', '"'], trim($string, '"'));
        $parsedString = preg_replace('/\\\\x[0-9a-f]{2}/i', '*', $parsedString);

        return $parsedString;
    }
}
