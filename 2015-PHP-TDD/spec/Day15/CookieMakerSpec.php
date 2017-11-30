<?php

namespace spec\AdventOfCode\Day15;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class CookieMakerSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day15\CookieMaker');
    }

    function it_calculates_the_correct_score_of_a_cookie_with_a_single_ingredient()
    {
        $this->addIngredient('Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8');

        $this->getBestCookieScore()->shouldReturn(0);
    }

    function it_calculates_the_best_cookie_score_for_multiple_ingredients()
    {
        $this->addIngredient('Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8');
        $this->addIngredient('Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3');

        $this->getBestCookieScore()->shouldReturn(62842880);
    }

    function it_calculates_the_best_cookie_score_for_multiple_ingredients_with_a_calorie_requirement()
    {
        $this->addIngredient('Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8');
        $this->addIngredient('Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3');

        $this->getBestCookieScore(500)->shouldReturn(57600000);
    }
}
