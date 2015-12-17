<?php
namespace AdventOfCode\Day15;

class CookieMaker
{
    private $ingredients = [];

    const MAX_QUANTITY = 100;

    public function addIngredient($ingredient)
    {
        list ($name, $properties) = explode(':', $ingredient);
        $this->ingredients[$name] = $this->parsePropertiesIntoArray($properties);
    }

    public function getBestCookieScore($calorieRequirement = 0)
    {
        return max(
            array_map(
                function ($combination) use ($calorieRequirement) {
                    return $this->getScoreForCombination($combination, $calorieRequirement);
                },
                $this->getIngredientCombinationArray($this->ingredients)
            )
        );
    }

    private function getIngredientCombinationArray($unusedIngredients, $quantitySoFar = 0)
    {
        if ($quantitySoFar == self::MAX_QUANTITY) {
            //Quantity all used up - have to return rest of ingredients with 0 quantity
            return [
                array_combine(array_keys($unusedIngredients), array_fill(0, count($unusedIngredients), 0))
            ];
        }

        reset($unusedIngredients);
        $thisIngredientName = key($unusedIngredients);
        unset($unusedIngredients[$thisIngredientName]);

        if (empty($unusedIngredients)) {
            //Only 1 option here - return the current ingredient with the remaining quantity
            return [
                [$thisIngredientName => self::MAX_QUANTITY - $quantitySoFar]
            ];
        }

        $combinationsArray = [];

        for ($quantity = 0; $quantity <= self::MAX_QUANTITY - $quantitySoFar; $quantity++) {
            $combinationsForRestOfIngredients =
                $this->getIngredientCombinationArray($unusedIngredients, $quantitySoFar + $quantity);

            foreach ($combinationsForRestOfIngredients as $combinationForRestOfIngredients) {
                $combinationsArray[] = array_merge([$thisIngredientName => $quantity], $combinationForRestOfIngredients);
            }
        }

        return $combinationsArray;
    }

    private function parsePropertiesIntoArray($properties)
    {
        $ingredientProperties = [];

        foreach (array_map('trim', explode(',', $properties)) as $property) {
            list($propertyName, $propertyValue) = explode(' ', $property);

            $ingredientProperties[$propertyName] = $propertyValue;
        }

        return $ingredientProperties;
    }

    private function getScoreForCombination($combination, $calorieRequirement = 0) {
        $capacity = 0;
        $durability = 0;
        $flavor = 0;
        $texture = 0;
        $calories = 0;

        foreach ($combination as $ingredientName => $quantity) {
            $capacity += $quantity * $this->ingredients[$ingredientName]['capacity'];
            $durability += $quantity * $this->ingredients[$ingredientName]['durability'];
            $flavor += $quantity * $this->ingredients[$ingredientName]['flavor'];
            $texture += $quantity * $this->ingredients[$ingredientName]['texture'];
            $calories += $quantity * $this->ingredients[$ingredientName]['calories'];
        }

        if ($calorieRequirement != 0 && ($calories != $calorieRequirement)) {
            return 0;
        }

        return max($capacity, 0)
            * max($durability, 0)
            * max($flavor, 0)
            * max($texture, 0);
    }
}
