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
            $wrappingPaperRequired += $this->getWrappingPaperRequirementForPresent($present);
        }

        return $wrappingPaperRequired;
    }

    public function getRequiredRibbonLength()
    {
        $ribbonLengthRequired = 0;

        foreach ($this->presents as $present) {
            $ribbonLengthRequired += $this->getRibbonLengthRequirementForPresent($present);
        }

        return $ribbonLengthRequired;
    }

    private function getWrappingPaperRequirementForPresent(Present $present)
    {
        return $present->getSurfaceArea() + $present->getAreaOfSmallestSize();
    }

    private function getRibbonLengthRequirementForPresent(Present $present)
    {
        return $present->getSmallestPerimeter() + $present->getVolume();
    }
}
