<?php
namespace spec\AdventOfCode\Day9;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class ShortestTripSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day9\ShortestTrip');
    }

    function it_calculates_the_shortest_distance_between_2_places()
    {
        $this->addDistance('London to Dublin = 464');

        $this->getShortestTrip()->shouldReturn(464);
    }

    function it_calculates_the_shortest_distance_between_3_places()
    {
        $this->addDistance('London to Dublin = 464');
        $this->addDistance('London to Belfast = 518');
        $this->addDistance('Dublin to Belfast = 141');

        $this->getShortestTrip()->shouldReturn(605);
    }
}
