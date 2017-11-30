<?php
namespace spec\AdventOfCode\Day4;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class StockingStufferSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day4\StockingStuffer');
    }

    function it_returns_the_digit_that_makes_an_md5_starting_with_5_zeros_1()
    {
        $this->findSuffixNumberFor('abcdef')->shouldReturn(609043);
    }

    function it_returns_the_digit_that_makes_an_md5_starting_with_5_zeros_2()
    {
        $this->findSuffixNumberFor('pqrstuv')->shouldReturn(1048970);
    }

    //Can't really write tests for more zeros as I don't have any examples until the code is written!
}
