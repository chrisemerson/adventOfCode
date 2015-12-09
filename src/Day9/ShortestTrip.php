<?php
namespace AdventOfCode\Day9;

class ShortestTrip
{
    private $distances = [];
    private $destinations = [];

    public function addDistance($distanceString)
    {
        list($places, $distance) = explode(' = ', $distanceString);
        list($place1, $place2) = explode(' to ', $places);

        $this->addDistanceBetween2Places($place1, $place2, $distance);
        $this->addDistanceBetween2Places($place2, $place1, $distance);

        $this->addDestination($place1);
        $this->addDestination($place2);
    }

    private function addDistanceBetween2Places($from, $to, $distance)
    {
        if (!isset($this->distances[$from])) {
            $this->distances[$from] = [];
        }

        $this->distances[$from][$to] = (int)$distance;
    }

    private function addDestination($place)
    {
        $this->destinations[] = $place;
        $this->destinations = array_unique($this->destinations);
    }

    public function getShortestTrip()
    {
        $minDistance = null;

        foreach ($this->destinations as $destination) {
            $destinationsWithoutThisOne = $this->removeArrayElement($this->destinations, $destination);

            $distanceFromThisDestination = $this->calculateShortestTrip(
                $destinationsWithoutThisOne,
                $destination,
                0
            );

            if (is_null($minDistance) || $distanceFromThisDestination < $minDistance) {
                $minDistance = $distanceFromThisDestination;
            }
        }

        return $minDistance;
    }

    private function calculateShortestTrip($unvisitedDestinations, $currentLocation, $distanceTravelledSoFar) {
        if (empty($unvisitedDestinations)) {
            return $distanceTravelledSoFar;
        }

        return min(
            array_map(
                function($destination) use ($unvisitedDestinations, $currentLocation, $distanceTravelledSoFar) {
                    return $this->calculateShortestTrip(
                        $this->removeArrayElement($unvisitedDestinations, $destination),
                        $destination,
                        $distanceTravelledSoFar + $this->getDistanceBetween($currentLocation, $destination)
                    );
                },
                $unvisitedDestinations
            )
        );
    }

    private function getDistanceBetween($from, $to) {
        return $this->distances[$from][$to];
    }

    private function removeArrayElement($array, $elementToRemove)
    {
        return array_filter(
            $array,
            function ($value) use ($elementToRemove) {
                return ($value != $elementToRemove);
            }
        );
    }
}
