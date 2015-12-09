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
        $this->addString('');

        $this->getDifferenceInCharactersForDecodedStrings()->shouldReturn(0);
    }

    function it_can_decode_empty_quotes()
    {
        $this->addString('""');

        $this->getDifferenceInCharactersForDecodedStrings()->shouldReturn(2);
    }

    function it_can_decode_a_simple_string_in_quotes()
    {
        $this->addString('"abc"');

        $this->getDifferenceInCharactersForDecodedStrings()->shouldReturn(2);
    }

    function it_can_decode_a_string_with_an_escaped_quote_in_it()
    {
        $this->addString('"aaa\\"aaa"');

        $this->getDifferenceInCharactersForDecodedStrings()->shouldReturn(3);
    }

    function it_can_decode_a_hexadecimal_character()
    {
        $this->addString('"\\x27"');
        $this->getDifferenceInCharactersForDecodedStrings()->shouldReturn(5);
    }

    function it_can_sum_the_difference_in_multiple_string_decodings()
    {
        $this->addString('"aaa\\"aaa"');
        $this->addString('"\x27"');

        $this->getDifferenceInCharactersForDecodedStrings()->shouldReturn(8);
    }

    function it_can_encode_an_empty_string()
    {
        $this->addString('');

        $this->getDifferenceInCharactersForEncodedStrings()->shouldReturn(2);
    }

    function it_can_encode_empty_quotes()
    {
        $this->addString('""');

        $this->getDifferenceInCharactersForEncodedStrings()->shouldReturn(4);
    }

    function it_can_encode_a_simple_string_in_quotes()
    {
        $this->addString('"abc"');

        $this->getDifferenceInCharactersForEncodedStrings()->shouldReturn(4);
    }

    function it_can_encode_a_string_with_an_escaped_quote_in_it()
    {
        $this->addString('"aaa\\"aaa"');

        $this->getDifferenceInCharactersForEncodedStrings()->shouldReturn(6);
    }

    function it_can_encode_a_hexadecimal_character()
    {
        $this->addString('"\\x27"');
        $this->getDifferenceInCharactersForEncodedStrings()->shouldReturn(5);
    }

    function it_can_sum_the_difference_in_multiple_string_encodings()
    {
        $this->addString('"aaa\\"aaa"');
        $this->addString('"\x27"');

        $this->getDifferenceInCharactersForEncodedStrings()->shouldReturn(11);
    }
}
