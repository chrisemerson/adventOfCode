<?php
namespace AdventOfCode\Day2;

class WrappingPaper
{
    /** @var Present[] */
    private $presents = [];

    public function addPresent(Present $present)
    {
        $this->presents[] = $present;
    }

    public function getRequiredWrappingPaper()
    {
        $wrappingPaperRequired = 0;

        foreach ($this->presents as $present) {
            $wrappingPaperRequired += $present->getSurfaceArea() + $present->getAreaOfSmallestSize();
        }

        return $wrappingPaperRequired;
    }
}
