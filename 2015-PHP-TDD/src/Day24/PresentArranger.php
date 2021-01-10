<?php
namespace AdventOfCode\Day24;

use Exception;

class PresentArranger
{
    private array $presents;

    public function addPresents(array $presents) {
        $this->presents = $presents;
    }

    public function findLowestQuantumEntaglementOfFirstGroup(int $numGroups)
    {
        $results = $this->findWaysToAddToGroupSize($this->getSizeOfEachGroup($numGroups), $this->presents);

        $minimumCount = min(
            array_map(
                fn ($x) => count($x),
                $results
            )
        );

        return min(
            array_map(
                function ($result) {
                    return array_reduce($result, fn($a, $b) => $a * $b, 1);
                },
                array_filter(
                    $results,
                    fn($x) => count($x) == $minimumCount
                )
            )
        );
    }

    private function findWaysToAddToGroupSize($requiredSum, $remainingPresents) {
        $waysToAddToGroupSize = [];

        if ($requiredSum == 0) {
            return [[]];
        } else if (count($remainingPresents) == 0) {
            throw new Exception();
        }

        foreach ($remainingPresents as $remainingPresent) {
            if ($remainingPresent <= $requiredSum) {
                try {
                    $innerResults = $this->findWaysToAddToGroupSize(
                        $requiredSum - $remainingPresent,
                        array_filter($remainingPresents, function ($present) use ($remainingPresent) {
                            return $present > $remainingPresent;
                        })
                    );

                    foreach ($innerResults as $innerResult) {
                        $waysToAddToGroupSize[] = array_merge([$remainingPresent], $innerResult);
                    }
                } catch (Exception $e) {
                    //Ignore this branch, can't get a result out of it
                }
            }
        }

        $waysToAddToGroupSize = array_filter(
            $waysToAddToGroupSize,
            function ($wayToAddToGroupSize) use ($requiredSum) {
                return array_sum($wayToAddToGroupSize) == $requiredSum;
            });

        if (count($waysToAddToGroupSize) == 0) {
            throw new Exception();
        }

        return $waysToAddToGroupSize;
    }

    private function getSizeOfEachGroup(int $numGroups): int
    {
        return array_sum($this->presents) / $numGroups;
    }
}
