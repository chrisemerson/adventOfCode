<?php
namespace spec\AdventOfCode\Day17;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class EggnogContainersSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day17\EggnogContainers');
    }

    function it_returns_1_combination_for_1_container()
    {
        $this->addContainer(10);

        $this->getCombinations(10)->shouldReturn(1);
    }

    function it_returns_1_combination_for_2_containers_that_add_up_to_the_required_total()
    {
        $this->addContainer(3);
        $this->addContainer(7);

        $this->getCombinations(10)->shouldReturn(1);
    }

    function it_returns_2_combinations_for_4_containers_that_add_up_to_requirement_in_2_different_ways()
    {
        $this->addContainer(3);
        $this->addContainer(4);
        $this->addContainer(6);
        $this->addContainer(7);

        $this->getCombinations(10)->shouldReturn(2);
    }
}
