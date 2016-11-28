<?php
namespace AdventOfCode\Day19;

class MedicineMachine
{
    private $molecule;
    private $transformations = [];
    private $foundTransformations = [];
    private static $minFoundSoFar = PHP_INT_MAX;

    public function setMolecule($molecule)
    {
        $this->molecule = $molecule;
    }

    public function addTransformation($transformation)
    {
        list($source, $destination) = explode(' => ', $transformation);

        if (!isset($this->transformations[$source])) {
            $this->transformations[$source] = [];
        }

        $this->transformations[$source][] = $destination;
    }

    public function getNumberOfDistinctMolecules()
    {
        foreach ($this->transformations as $source => $possibleTransformations) {
            for ($offset = 0; ($index = strpos($this->molecule, $source, $offset)) !== false; $offset = $index + 1) {
                foreach ($possibleTransformations as $possibleTransformation) {
                    $newString = substr_replace($this->molecule, $possibleTransformation, $index, strlen($source));
                    $this->foundTransformations[] = $newString;
                }
            }
        }

        return count(array_unique($this->foundTransformations));
    }

    public function findQuickestRouteToMolecule($startingMolecule)
    {
        return $this->applyTransformationsToReachTarget($startingMolecule, true);
    }

    private function applyTransformationsToReachTarget($currentMolecule)
    {
        if ($currentMolecule == $this->molecule) {
            return 0;
        }

        $minSteps = PHP_INT_MAX;

        $transformationsNormalised = [];

        foreach ($this->transformations as $source => $transformationsForSource) {
            foreach ($transformationsForSource as $transformation) {
                $transformationsNormalised[] = ['source' => $source, 'transformation' => $transformation];
            }
        }

        usort(
            $transformationsNormalised,
            function ($a, $b) {
                return strlen($b['transformation']) - strlen($a['transformation']);
            }
        );

        foreach ($transformationsNormalised as $transformationNormalised) {
            $source = $transformationNormalised['source'];
            $transformation = $transformationNormalised['transformation'];

            for ($offset = 0; ($index = strpos($currentMolecule, $transformation, $offset)) !== false; $offset = $index + 1) {
                if ($minSteps == PHP_INT_MAX || $minSteps < self::$minFoundSoFar) {
                    return $this->applyTransformationsToReachTarget(
                        substr_replace(
                            $currentMolecule,
                            $source,
                            $index,
                            strlen($transformation)
                        )
                    ) + 1;
                }
            }
        }

        return $minSteps;
    }
}
