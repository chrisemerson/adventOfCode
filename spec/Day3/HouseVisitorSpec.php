<?php

namespace spec\AdventOfCode\Day3;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class HouseVisitorSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day3\HouseVisitor');
    }

    function it_returns_1_house_visited_at_the_start()
    {
        $this->getNumberOfHousesVisited()->shouldReturn(1);
    }

    function it_returns_2_houses_visited_after_a_single_direction()
    {
        $this->visitHouses('>');
        $this->getNumberOfHousesVisited()->shouldReturn(2);
    }

    function it_returns_number_of_houses_visited_after_string_of_directions_that_visit_each_house_once()
    {
        $this->visitHouses('>^<^>>>v');
        $this->getNumberOfHousesVisited()->shouldReturn(9);
    }

    function it_returns_number_of_houses_visited_after_revisiting_start_location()
    {
        $this->visitHouses('><');
        $this->getNumberOfHousesVisited()->shouldReturn(2);
    }

    function it_returns_number_of_houses_when_travelling_in_a_square()
    {
        $this->visitHouses('^>v<');
        $this->getNumberOfHousesVisited()->shouldReturn(4);
    }

    function it_returns_number_of_houses_visited_after_complex_pattern()
    {
        $this->visitHouses('>>^^<<vvvv<<>>^^');
        $this->getNumberOfHousesVisited()->shouldReturn(12);
    }
}
