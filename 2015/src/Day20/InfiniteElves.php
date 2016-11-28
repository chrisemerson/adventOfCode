<?php
namespace AdventOfCode\Day20;

class InfiniteElves
{
    private $elfDeliveringLimit;
    private $presentsPerHouse = 10;

    public function setElfDeliveringLimit($elfDeliveringLimit)
    {
        $this->elfDeliveringLimit = $elfDeliveringLimit;
    }

    public function setNumberOfPresentsPerHouse($presentsPerHouse)
    {
        $this->presentsPerHouse = $presentsPerHouse;
    }

    public function getLowestHouseWithThisNumberOfPresents($presents)
    {
        for ($houseNo = 1; $this->getNumberOfPresentsForHouse($houseNo) < $presents; $houseNo++);

        return $houseNo;
    }

    public function getNumberOfPresentsForHouse($houseNumber)
    {
        return $this->presentsPerHouse * (int) array_sum(
            $this->getUniqueFactors($houseNumber)
        );
    }

    private function getUniqueFactors($houseNumber)
    {
        $factors = [];

        foreach (range(1, floor(sqrt($houseNumber))) as $factorCandidate) {
            if ($houseNumber % $factorCandidate == 0) {
                if ($this->elfDeliveringLimit == 0 || $factorCandidate <= $this->elfDeliveringLimit) {
                    $factors[] = $houseNumber / $factorCandidate;
                }

                if ($this->elfDeliveringLimit == 0 || $houseNumber / $factorCandidate <= $this->elfDeliveringLimit) {
                    $factors[] = $factorCandidate;
                }
            }
        }

        return array_unique($factors);
    }
}
