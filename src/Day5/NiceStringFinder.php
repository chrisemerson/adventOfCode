<?php
namespace AdventOfCode\Day5;

class NiceStringFinder
{
    private $naughtyWords = 0;
    private $niceWords = 0;

    const VOWELS = ['a', 'e', 'i', 'o', 'u'];
    const NAUGHTY_SUBSTRINGS = ['ab', 'cd', 'pq', 'xy'];

    public function addWord($word)
    {
        if ($this->wordIsNice($word)
        ) {
            $this->niceWords++;
        } else {
            $this->naughtyWords++;
        }
    }

    public function getNumberOfNiceWords()
    {
        return $this->niceWords;
    }

    public function getNumberOfNaughtyWords()
    {
        return $this->naughtyWords;
    }

    private function wordIsNice($word)
    {
        return
            $this->wordContainsAtLeast3Vowels($word)
            && $this->wordContainsDoubleLetter($word)
            && $this->wordDoesntContainANaughtySubString($word);
    }

    private function wordContainsAtLeast3Vowels($word)
    {
        return (substr_count(str_replace(self::VOWELS, '*', $word), '*') >= 3);
    }

    private function wordContainsDoubleLetter($word)
    {
        $doubleLetterFound = false;

        for ($i = 1; $i < strlen($word); $i++) {
            if ($word{$i} == $word{$i - 1}) {
                $doubleLetterFound = true;
            }
        }
        return $doubleLetterFound;
    }

    private function wordDoesntContainANaughtySubString($word)
    {
        return (substr_count(str_replace(self::NAUGHTY_SUBSTRINGS, '*', $word), '*') == 0);
    }
}
