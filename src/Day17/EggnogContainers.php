<?php
namespace AdventOfCode\Day17;

class EggnogContainers
{
    private $containers = [];

    public function addContainer($capacity)
    {
        $this->containers[] = $capacity;
    }

    public function getCombinations($totalCapacity)
    {
        return 1;
    }
}
