<?php
namespace spec\AdventOfCode\Day1;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class NotQuiteLispSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day1\NotQuiteLisp');
    }

    function it_should_start_on_floor_0()
    {
        $this->getFloor()->shouldReturn(0);
    }

    function it_should_move_up_a_floor_when_input_is_open_parens()
    {
        $this->move("(");
        $this->getFloor()->shouldReturn(1);
    }

    function it_should_move_down_a_floor_when_input_is_close_parens()
    {
        $this->move(")");
        $this->getFloor()->shouldReturn(-1);
    }

    function it_should_work_out_the_floor_from_a_combination_of_moves()
    {
        $this->move("()");
        $this->getFloor()->shouldReturn(0);
    }

    function it_should_work_out_the_floor_from_a_complex_set_of_moves()
    {
        $this->move("(()((()()))()()())(())))())()))()");
        $this->getFloor()->shouldReturn(-5);
    }

    function it_returns_1_for_the_first_basement_move_when_moving_down_immediately()
    {
        $this->move(")");
        $this->getFirstBasementMove()->shouldReturn(1);
    }

    function it_calculates_the_first_basement_move_after_some_other_previous_moves()
    {
        $this->move("(())()(()()))");
        $this->getFirstBasementMove()->shouldReturn(13);
    }

    function it_ignores_subsequent_trips_into_the_basement()
    {
        $this->move("())(())(");
        $this->getFirstBasementMove()->shouldReturn(3);
    }
}
