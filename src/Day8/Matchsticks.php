<?php

namespace AdventOfCode\Day8;

class Matchsticks
{
    private $strings;

    public function addString($string)
    {
        $this->strings[] = $string;
    }

    public function getDifferenceInCharactersForDecodedStrings()
    {
        $difference = 0;

        foreach ($this->strings as $string) {
            $decodedString = $this->decodeString($string);

            $difference += (strlen($string) - strlen($decodedString));
        }

        return $difference;
    }

    public function getDifferenceInCharactersForEncodedStrings()
    {
        $difference = 0;

        foreach ($this->strings as $string) {
            $encodedString = $this->encodeString($string);

            $difference += (strlen($encodedString) - strlen($string));
        }

        return $difference;
    }

    private function decodeString($string)
    {
        $decodedString = str_replace(['\\\\', '\\"'], ['\\', '"'], trim($string, '"'));
        $decodedString = preg_replace('/\\\\x[0-9a-f]{2}/i', '*', $decodedString);

        return $decodedString;
    }

    private function encodeString($string)
    {
        $encodedString = str_replace(['\\', '"'], ['\\\\', '\\"'], $string);

        return '"' . $encodedString . '"';
    }
}
