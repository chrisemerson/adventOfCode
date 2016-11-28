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
        $combinations = $this->findCombinations(array_keys($this->containers), $totalCapacity);

        $combinations = array_map(
            function($item) {
                sort($item);
                return implode('-', $item);
            },
            $combinations
        );

        return count(array_unique($combinations));
    }

    public function getUsingMinCombinations($totalCapacity)
    {
        $combinations = $this->findCombinations(array_keys($this->containers), $totalCapacity);

        $minContainers = min(
            array_map(
                function ($item) {
                    return count($item);
                },
                $combinations
            )
        );

        $combinations = array_filter(
            $combinations,
            function ($item) use ($minContainers) {
                return count($item) == $minContainers;
            }
        );

        $combinations = array_map(
            function($item) {
                sort($item);
                return implode('-', $item);
            },
            $combinations
        );

        return count(array_unique($combinations));
    }

    private function findCombinations($remainingContainers, $remainingCapacity, $indent = "")
    {
        $return = [];

        if ($remainingCapacity == 0) {
            $return[] = [];
        }

        if (count($remainingContainers) == 1) {
            $onlyRemainingContainer = array_values($remainingContainers)[0];

            if ($this->getContainerCapacity($onlyRemainingContainer) == $remainingCapacity) {
                $return[] = [$onlyRemainingContainer];
            }
        } else if (count($remainingContainers) > 1) {
            foreach ($remainingContainers as $remainingContainer) {
                if ($this->getContainerCapacity($remainingContainer) <= $remainingCapacity) {
                    $return = array_merge(
                        $return,
                        array_map(
                            function($item) use ($remainingContainer) {
                                return array_merge([$remainingContainer], array_values($item));
                            },
                            $this->findCombinations(
                                $this->removeArrayElement($remainingContainers, $remainingContainer),
                                $remainingCapacity - $this->getContainerCapacity($remainingContainer),
                                $indent . "    "
                            )
                        )
                    );
                }
            }
        }

        return $return;
    }

    private function removeArrayElement($array, $valueToRemove) {
        unset($array[array_search($valueToRemove, $array)]);

        return $array;
    }

    private function getContainerCapacity($containerKey)
    {
        return $this->containers[$containerKey];
    }
}
