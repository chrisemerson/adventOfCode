<?php
namespace spec\AdventOfCode\Day12;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class NumberFinderSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day12\NumberFinder');
    }

    function it_returns_0_for_an_empty_string()
    {
        $this->getNumberTotal('')->shouldReturn(0);
    }

    function it_returns_0_for_an_empty_json_array()
    {
        $this->getNumberTotal('[]')->shouldReturn(0);
    }

    function it_returns_a_number_literal_as_itself()
    {
        $this->getNumberTotal('23')->shouldReturn(23);
    }

    function it_adds_together_numbers_in_the_input_string()
    {
        $this->getNumberTotal('[1,2,3]')->shouldReturn(6);
    }

    function it_can_add_negative_numbers()
    {
        $this->getNumberTotal('{"a":{"b":4},"c":-1}')->shouldReturn(3);
    }
}
