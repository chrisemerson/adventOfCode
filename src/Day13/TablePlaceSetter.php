<?php
namespace AdventOfCode\Day13;

class TablePlaceSetter
{
    const INPUT_REGEX = '/^(.*) would (gain|lose) (\d+) happiness units by sitting next to (.*)\.$/';
    const SEPARATOR = ".";

    private $preferences = [];

    public function addSeatingPreference($preference)
    {
        if (preg_match(self::INPUT_REGEX, $preference, $matches)) {
            list(, $subjectPerson, $gainLose, $points, $objectPerson) = $matches;

            if ($gainLose == "lose") {
                $points = 0 - $points;
            }

            if (!isset($this->preferences[$subjectPerson])) {
                $this->preferences[$subjectPerson] = [];
            }

            $this->preferences[$subjectPerson][$objectPerson] = $points;
        }
    }

    public function addMyself()
    {
        foreach (array_keys($this->preferences) as $person) {
            $this->addSeatingPreference("$person would gain 0 happiness units by sitting next to Me.");
            $this->addSeatingPreference("Me would gain 0 happiness units by sitting next to $person.");
        }
    }

    public function getMaximumPossibleHappinessRating()
    {
        if (count($this->preferences) == 0) {
            return 0;
        }

        return max(
            array_map(
                function ($arrangement) {
                    return $this->getHappinessRatingForArrangement($arrangement);
                },
                $this->getAllArrangementsOfPeople(array_keys($this->preferences))
            )
        );
    }

    private function getAllArrangementsOfPeople($people)
    {
        $arrangements = [];

        if (count($people) == 1) {
            $arrangements[] = reset($people);
        } else {
            foreach ($people as $person) {
                $arrangements = array_merge(
                    $arrangements,
                    array_map(
                        function($arrangement) use ($person) {
                            return $person . self::SEPARATOR . $arrangement;
                        },
                        $this->getAllArrangementsOfPeople($this->removeArrayValue($people, $person))
                    )
                );
            }
        }

        return $arrangements;
    }

    private function removeArrayValue($array, $value)
    {
        unset($array[array_search($value, $array)]);

        return $array;
    }

    private function getHappinessRatingForArrangement($arrangement)
    {
        $arrangementArray = explode(self::SEPARATOR, $arrangement);
        $happinessRating = 0;

        for ($i = 1; $i < count($arrangementArray); $i++) {
            $happinessRating +=
                $this->preferences[$arrangementArray[$i - 1]][$arrangementArray[$i]]
                + $this->preferences[$arrangementArray[$i]][$arrangementArray[$i - 1]];
        }

        if (count($arrangementArray) > 2) {
            $happinessRating +=
                $this->preferences[$arrangementArray[0]][$arrangementArray[count($arrangementArray) - 1]]
                + $this->preferences[$arrangementArray[count($arrangementArray) - 1]][$arrangementArray[0]];
        }

        return $happinessRating;
    }
}
