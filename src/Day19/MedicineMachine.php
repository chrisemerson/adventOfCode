<?php
namespace AdventOfCode\Day19;

class MedicineMachine
{
    private $molecule;
    private $transformations = [];
    private $foundTransformations = [];

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
        return min($this->applyTransformationsToReachTarget($startingMolecule));
    }

    private function applyTransformationsToReachTarget($currentMolecule, $noSteps = 0)
    {
        $return = [];

        if ($currentMolecule == $this->molecule) {
            $return[] = $noSteps;
        }

        foreach ($this->transformations as $source => $transformationsForSource) {
            foreach ($transformationsForSource as $transformation) {
                for ($offset = 0; ($index = strpos($currentMolecule, $source, $offset)) !== false; $offset = $index + 1) {
                    $newString = substr_replace($currentMolecule, $transformation, $index, strlen($source));

                    if (strlen($newString) <= strlen($this->molecule)) {
                        $return = array_merge($return, $this->applyTransformationsToReachTarget($newString, $noSteps + 1));
                    }
                }
            }
        }

        return $return;
    }
}
