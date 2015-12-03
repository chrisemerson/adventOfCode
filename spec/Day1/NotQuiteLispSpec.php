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
}
