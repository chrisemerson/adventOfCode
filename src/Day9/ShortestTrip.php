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

        $this->distances[$from][$to] = (int) $distance;
    }

    private function addDestination($place)
    {
        $this->destinations[] = $place;
        $this->destinations = array_unique($this->destinations);
    }

    public function getShortestTrip()
    {
        return $this->calculateTrip($this->destinations);
    }

    public function getLongestTrip()
    {
        return $this->calculateTrip($this->destinations, '', 0, 'max');
    }

    private function calculateTrip(
        $unvisitedDestinations,
        $currentLocation = '',
        $distanceTravelledSoFar = 0,
        $reductionFunction = 'min'
    ) {
        if (empty($unvisitedDestinations)) {
            return $distanceTravelledSoFar;
        }

        return call_user_func(
            $reductionFunction,
            array_map(
                function(
                    $destination
                ) use (
                    $unvisitedDestinations,
                    $currentLocation,
                    $distanceTravelledSoFar,
                    $reductionFunction
                ) {
                    $maxTrip = $this->calculateTrip(
                        $this->removeArrayElement($unvisitedDestinations, $destination),
                        $destination,
                        $distanceTravelledSoFar + $this->getDistanceBetween($currentLocation, $destination),
                        $reductionFunction
                    );

                    return $maxTrip;
                },
                $unvisitedDestinations
            )
        );
    }

    private function removeArrayElement($array, $valueToRemove)
    {
        unset($array[array_search($valueToRemove, $array)]);

        return $array;
    }

    private function getDistanceBetween($from, $to) {
        if (empty($from) || empty($to)) {
            return 0;
        }

        return $this->distances[$from][$to];
    }
}
