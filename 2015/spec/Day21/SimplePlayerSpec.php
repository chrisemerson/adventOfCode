<?php
namespace spec\AdventOfCode\Day21;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class SimplePlayerSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith(1, 1, 1);
    }

    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day21\SimplePlayer');
    }

    function it_reports_a_player_who_has_not_been_attacked_as_undefeated()
    {
        $this->isDefeated()->shouldReturn(false);
    }

    function it_defeats_the_player()
    {
        $this->attack(1);

        $this->isDefeated()->shouldReturn(true);
    }

    function it_can_play_a_series_of_moves()
    {
        $this->beConstructedWith(8, 5, 5);

        $this->isDefeated()->shouldReturn(false);

        $this->attack(2);
        $this->isDefeated()->shouldReturn(false);

        $this->attack(2);
        $this->isDefeated()->shouldReturn(false);

        $this->attack(2);
        $this->isDefeated()->shouldReturn(false);

        $this->attack(2);
        $this->isDefeated()->shouldReturn(true);
    }
}
