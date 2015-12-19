<?php
namespace spec\AdventOfCode\Day18;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class GameOfLifeSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day18\GameOfLife');
    }

    function it_reports_no_lights_on_when_initial_state_is_all_off()
    {
        $this->setInitialState("......
......
......
......
......
......");

        $this->getNumberOfLightsTurnedOn()->shouldReturn(0);
    }

    function it_reports_the_number_of_lights_on_when_initial_state_is_a_mixture()
    {
        $this->setInitialState(".#..#.
......
...#..
......
......
.#..#.");

        $this->getNumberOfLightsTurnedOn()->shouldReturn(5);
    }

    function it_turns_off_a_single_light_after_1_iteration()
    {
        $this->setInitialState("......
......
...#..
......
......
......");

        $this->iterate();

        $this->getNumberOfLightsTurnedOn()->shouldReturn(0);
    }

    function it_leaves_on_a_light_with_2_neighbours()
    {
        $this->setInitialState("......
......
...#..
..#...
.#....
......");

        $this->iterate();

        $this->getNumberOfLightsTurnedOn()->shouldReturn(1);
    }

    function it_turns_on_a_light_with_3_neighbours()
    {
        $this->setInitialState("......
......
..##..
......
.#....
......");

        $this->iterate();

        $this->getNumberOfLightsTurnedOn()->shouldReturn(1);
    }

    function it_can_iterate_lights_on_the_edge()
    {
        $this->setInitialState("......
......
#.....
#.....
#.....
......");

        $this->iterate();

        $this->getNumberOfLightsTurnedOn()->shouldReturn(2);
    }

    function it_iterates_correctly_multiple_times()
    {
        $this->setInitialState(".#.#.#
...##.
#....#
..#...
#.#..#
####..");

        $this->iterate();

        $this->getNumberOfLightsTurnedOn()->shouldReturn(11);

        $this->iterate();

        $this->getNumberOfLightsTurnedOn()->shouldReturn(8);

        $this->iterate();

        $this->getNumberOfLightsTurnedOn()->shouldReturn(4);

        $this->iterate();

        $this->getNumberOfLightsTurnedOn()->shouldReturn(4);
    }
}
