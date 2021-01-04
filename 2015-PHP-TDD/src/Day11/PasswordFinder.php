<?php
namespace AdventOfCode\Day11;

class PasswordFinder
{
    const VALID_CHARS = [
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'j', 'k', 'm',
        'n', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
    ];

    public function getNextPassword($currentPassword)
    {
        $password = $this->removeAmbiguousLetters($currentPassword);

        do {
            $password = $this->iteratePassword($password);
        } while (!$this->isValidPassword($password));

        return $password;
    }

    private function isValidPassword($password)
    {
        $contains3ConsecutiveLetters = $this->contains3ConsecutiveLetters($password);
        $contains2NonOverlappingPairsOfLetters = $this->contains2NonOverlappingPairsOfLetters($password);

        return $contains3ConsecutiveLetters && $contains2NonOverlappingPairsOfLetters;
    }

    private function iteratePassword($password, $charToIterate = 7)
    {
        if ($charToIterate == -1) {
            return $password;
        }

        $nextIndex = array_search($password[$charToIterate], self::VALID_CHARS) + 1;

        if ($nextIndex >= count(self::VALID_CHARS)) {
            $password = $this->iteratePassword($password, $charToIterate - 1);
            $nextIndex = 0;
        }

        $password[$charToIterate] = self::VALID_CHARS[$nextIndex];

        return $password;
    }

    private function contains3ConsecutiveLetters($password)
    {
        $consecutiveCharsFound = false;

        for ($i = 0; $i <= 5; $i++) {
            if (
                ord($password[$i + 1]) == ord($password[$i]) + 1
                && ord($password[$i + 2]) == ord($password[$i]) + 2
            ) {
                $consecutiveCharsFound = true;
            }
        }

        return $consecutiveCharsFound;
    }

    private function contains2NonOverlappingPairsOfLetters($password)
    {
        $contains2NonOverlappingPairsOfLetters = false;

        if (preg_match('/(.)\1.*(.)\2/i', $password)) {
            $contains2NonOverlappingPairsOfLetters = true;
            return $contains2NonOverlappingPairsOfLetters;
        }
        return $contains2NonOverlappingPairsOfLetters;
    }

    private function removeAmbiguousLetters($password)
    {
        $ambiguousLetters = ['i', 'l', 'o'];

        for ($i = 0; $i <=7; $i++) {
            if (in_array($password[$i], $ambiguousLetters)) {
                return
                    substr($password, 0, $i)
                    . chr(ord($password[$i]) + 1)
                    . str_repeat('a', 7 - $i);
            }
        }

        return $password;
    }
}
