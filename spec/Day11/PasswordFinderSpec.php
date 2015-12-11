<?php
namespace spec\AdventOfCode\Day11;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class PasswordFinderSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day11\PasswordFinder');
    }

    function it_returns_the_next_password_when_the_next_iteration_is_valid()
    {
        $this->getNextPassword('qaabbxyy')->shouldReturn('qaabbxyz');
    }

    function it_iterates_until_a_password_with_3_consecutive_letters_is_found()
    {
        $this->getNextPassword('ffgbbaba')->shouldReturn('ffgbbabc');
    }

    function it_wraps_iteration_to_the_next_character_when_reaching_the_end_of_the_alphabet()
    {
        $this->getNextPassword('ffhhgaaz')->shouldReturn('ffhhgabc');
    }

    function it_wraps_everything_around_back_to_the_start()
    {
        $this->getNextPassword('zzzzzzzz')->shouldReturn('aaaaaabc');
    }

    function it_returns_passwords_with_2_non_overlapping_pairs_of_letters()
    {
        $this->getNextPassword('fghedcba')->shouldReturn('fgheddaa');
    }

    function it_returns_passwords_that_dont_have_ambiguous_letters()
    {
        $this->getNextPassword('aacceghh')->shouldReturn('aaccepqr');
    }

    function it_follows_the_examples_on_the_specifications()
    {
        $this->getNextPassword('abcdefgh')->shouldReturn('abcdffaa');
        $this->getNextPassword('ghijklmn')->shouldReturn('ghjaabcc');
    }
}
