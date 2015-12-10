<?php
namespace spec\AdventOfCode\Day10;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class LookAndSaySpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day10\LookAndSay');
    }

    function it_handles_a_single_1_as_input()
    {
        $this->lookAndSay('1')->shouldReturn('11');
    }

    function it_handles_strings_of_greater_than_length_1()
    {
        $this->lookAndSay('11')->shouldReturn('21');
    }

    function it_handles_strings_with_more_than_one_number()
    {
        $this->lookAndSay('12')->shouldReturn('1112');
    }

    function it_can_iterate_the_process_multiple_times()
    {
        $this->lookAndSay('12', 2)->shouldReturn('3112');
    }
}
