<?php
namespace AdventOfCode\Day5;

class NiceStringFinder
{
    private $naughtyStrings = 0;
    private $niceStrings = 0;

    private $naughtyStringsNewModel = 0;
    private $niceStringsNewModel = 0;

    const VOWELS = ['a', 'e', 'i', 'o', 'u'];
    const NAUGHTY_SUBSTRINGS = ['ab', 'cd', 'pq', 'xy'];

    public function addString($string)
    {
        if ($this->stringIsNice($string)
        ) {
            $this->niceStrings++;
        } else {
            $this->naughtyStrings++;
        }

        if ($this->stringIsNiceInNewModel($string)) {
            $this->niceStringsNewModel++;
        } else {
            $this->naughtyStringsNewModel++;
        }
    }

    public function getNumberOfNiceStrings()
    {
        return $this->niceStrings;
    }

    public function getNumberOfNaughtyStrings()
    {
        return $this->naughtyStrings;
    }

    public function getNumberOfNiceStringsInNewModel()
    {
        return $this->niceStringsNewModel;
    }

    public function getNumberOfNaughtyStringsInNewModel()
    {
        return $this->naughtyStringsNewModel;
    }

    private function stringIsNice($string)
    {
        return
            $this->stringContainsAtLeast3Vowels($string)
            && $this->stringContainsDoubleLetter($string)
            && $this->stringDoesntContainANaughtySubString($string);
    }

    private function stringIsNiceInNewModel($string)
    {
        return $this->stringContainsRepeatingPair($string) && $this->stringContainsSandwichedLetter($string);
    }

    private function stringContainsAtLeast3Vowels($string)
    {
        return (substr_count(str_replace(self::VOWELS, '*', $string), '*') >= 3);
    }

    private function stringContainsDoubleLetter($string)
    {
        $doubleLetterFound = false;

        for ($i = 1; $i < strlen($string); $i++) {
            if ($string{$i} == $string{$i - 1}) {
                $doubleLetterFound = true;
            }
        }
        return $doubleLetterFound;
    }

    private function stringDoesntContainANaughtySubString($string)
    {
        return (substr_count(str_replace(self::NAUGHTY_SUBSTRINGS, '*', $string), '*') == 0);
    }

    private function stringContainsSandwichedLetter($string)
    {
        return preg_match('/(.).\1/', $string);
    }

    private function stringContainsRepeatingPair($string)
    {
        return preg_match('/(..).*\1/', $string);
    }
}
