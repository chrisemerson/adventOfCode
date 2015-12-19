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
            $offset = 0;

            while (($index = strpos($this->molecule, $source, $offset)) !== false) {
                foreach ($possibleTransformations as $possibleTransformation) {
                    $newString = substr_replace($this->molecule, $possibleTransformation, $index, strlen($source));
                    $this->foundTransformations[] = $newString;
                }

                $offset = $index + 1;
            }
        }

        return count(array_unique($this->foundTransformations));
    }
}
