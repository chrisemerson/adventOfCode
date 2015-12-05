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
        $this->getNumberOfNiceStrings()->shouldReturn(0);
    }

    function it_returns_0_naughty_words_with_no_input()
    {
        $this->getNumberOfNaughtyStrings()->shouldReturn(0);
    }

    function it_detects_words_with_less_than_3_vowels_as_naughty()
    {
        $this->addString('aebbt');

        $this->getNumberOfNiceStrings()->shouldReturn(0);
        $this->getNumberOfNaughtyStrings()->shouldReturn(1);
    }

    function it_detects_words_with_no_repeating_letters_in_a_row_as_naughty()
    {
        $this->addString('adego');

        $this->getNumberOfNiceStrings()->shouldReturn(0);
        $this->getNumberOfNaughtyStrings()->shouldReturn(1);
    }

    function it_detects_words_with_the_naughty_substrings_as_naughty()
    {
        $this->addString('abboe');
        $this->addString('cdeei');
        $this->addString('pqqeur');
        $this->addString('xyxxaoe');

        $this->getNumberOfNiceStrings()->shouldReturn(0);
        $this->getNumberOfNaughtyStrings()->shouldReturn(4);
    }

    function it_detects_nice_words_as_nice()
    {
        $this->addString('ugknbfddgicrmopn');
        $this->addString('aaa');

        $this->getNumberOfNiceStrings()->shouldReturn(2);
        $this->getNumberOfNaughtyStrings()->shouldReturn(0);
    }

    function it_detects_naughty_words_as_naughty()
    {
        $this->addString('jchzalrnumimnmhp');
        $this->addString('haegwjzuvuyypxyu');
        $this->addString('dvszwmarrgswjxmb');

        $this->getNumberOfNiceStrings()->shouldReturn(0);
        $this->getNumberOfNaughtyStrings()->shouldReturn(3);
    }

    function it_returns_0_nice_words_with_no_input_in_the_new_model()
    {
        $this->getNumberOfNiceStringsInNewModel()->shouldReturn(0);
    }

    function it_returns_0_naughty_words_with_no_input_in_the_new_model()
    {
        $this->getNumberOfNaughtyStringsInNewModel()->shouldReturn(0);
    }

    function it_detects_words_without_a_repeating_pair_as_naughty_in_the_new_model()
    {
        $this->addString('abcdefeghij');

        $this->getNumberOfNiceStringsInNewModel()->shouldReturn(0);
        $this->getNumberOfNaughtyStringsInNewModel()->shouldReturn(1);
    }

    function it_detects_words_with_no_sandwiched_letters_as_naughty_in_the_new_model()
    {
        $this->addString('uurcxstgmygtbstg');

        $this->getNumberOfNiceStringsInNewModel()->shouldReturn(0);
        $this->getNumberOfNaughtyStringsInNewModel()->shouldReturn(1);
    }

    function it_detects_nice_words_as_nice_in_the_new_model()
    {
        $this->addString('qjhvhtzxzqqjkmpb');
        $this->addString('xxyxx');

        $this->getNumberOfNiceStringsInNewModel()->shouldReturn(2);
        $this->getNumberOfNaughtyStringsInNewModel()->shouldreturn(0);
    }

    function it_detects_naughty_words_as_naughty_in_the_new_model()
    {
        $this->addString('uurcxstgmygtbstg');
        $this->addString('ieodomkazucvgmuy');

        $this->getNumberOfNiceStringsInNewModel()->shouldReturn(0);
        $this->getNumberOfNaughtyStringsInNewModel()->shouldreturn(2);
    }
}
