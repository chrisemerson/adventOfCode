<?php

namespace AdventOfCode\Day12;

class NumberFinder
{
    public function getNumberTotal($inputString)
    {
        $total = 0;
        $inputJSON = json_decode($inputString);

        if (is_object($inputJSON)) {
            $total += $this->getTotalForJSONObject($inputJSON);
        } elseif (is_array($inputJSON)) {
            $total += $this->getTotalForJSONArray($inputJSON);
        } else {
            $total += intval($inputJSON);
        }

        return $total;
    }

    private function getTotalForJSONObject($inputJSON)
    {
        $total = 0;

        foreach ($inputJSON as $element) {
            $total += $this->getNumberTotal(json_encode($element));

            if (is_string($element) && $element == "red") {
                return 0;
            }
        }

        return $total;
    }

    private function getTotalForJSONArray($inputJSON)
    {
        $total = 0;

        foreach ($inputJSON as $element) {
            $total += $this->getNumberTotal(json_encode($element));
        }

        return $total;
    }
}
