<?php
namespace spec\AdventOfCode\Day5;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class NiceStringFinderSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day5\NiceStringFinder');
    }

    function it_returns_0_nice_words_with_no_input()
    {
        $this->getNumberOfNiceWords()->shouldReturn(0);
    }

    function it_returns_0_naughty_words_with_no_input()
    {
        $this->getNumberOfNaughtyWords()->shouldReturn(0);
    }

    function it_detects_words_with_less_than_3_vowels_as_naughty()
    {
        $this->addWord('ae');

        $this->getNumberOfNiceWords()->shouldReturn(0);
        $this->getNumberOfNaughtyWords()->shouldReturn(1);
    }

    function it_detects_words_with_no_repeating_letters_in_a_row_as_naughty()
    {
        $this->addWord('adego');

        $this->getNumberOfNiceWords()->shouldReturn(0);
        $this->getNumberOfNaughtyWords()->shouldReturn(1);
    }

    function it_detects_words_with_the_naughty_substrings_as_naughty()
    {
        $this->addWord('ab');
        $this->addWord('cd');
        $this->addWord('pq');
        $this->addWord('xy');

        $this->getNumberOfNiceWords()->shouldReturn(0);
        $this->getNumberOfNaughtyWords()->shouldReturn(4);
    }

    function it_detects_nice_words_as_nice()
    {
        $this->addWord('ugknbfddgicrmopn');

        $this->getNumberOfNiceWords()->shouldReturn(1);
        $this->getNumberOfNaughtyWords()->shouldReturn(0);
    }
}
