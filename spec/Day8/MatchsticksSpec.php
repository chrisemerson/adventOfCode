<?php
namespace spec\AdventOfCode\Day8;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class MatchsticksSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day8\Matchsticks');
    }

    function it_can_decode_an_empty_string()
    {
        $this->addString("");

        $this->getDifferenceInCharacters()->shouldReturn(0);
    }

    function it_can_decode_empty_quotes()
    {
        $this->addString("\"\"");

        $this->getDifferenceInCharacters()->shouldReturn(2);
    }

    function it_can_decode_a_simple_string_in_quotes()
    {
        $this->addString("\"abc\"");

        $this->getDifferenceInCharacters()->shouldReturn(2);
    }
}
