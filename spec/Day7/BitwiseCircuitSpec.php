<?php

namespace spec\AdventOfCode\Day7;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class BitwiseCircuitSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day7\BitwiseCircuit');
    }

    function it_can_assign_a_signal_to_a_wire()
    {
        $this->addWiring('123 -> x');

        $this->getWireValue('x')->shouldBe(123);
    }

    function it_can_store_different_values_against_different_wires()
    {
        $this->addWiring('123 -> x');
        $this->addWiring('456 -> y');

        $this->getWireValue('x')->shouldBe(123);
        $this->getWireValue('y')->shouldBe(456);
    }

    function it_can_perform_a_bitwise_and_between_2_wires()
    {
        $this->addWiring('123 -> x');
        $this->addWiring('456 -> y');
        $this->addWiring('x AND y -> d');

        $this->getWireValue('d')->shouldBe(72);
    }

    function it_can_perform_the_wiring_in_any_order()
    {
        $this->addWiring('x AND y -> d');
        $this->addWiring('123 -> x');
        $this->addWiring('456 -> y');

        $this->getWireValue('d')->shouldBe(72);
    }

    function it_can_perform_a_bitwise_or_between_2_wires()
    {
        $this->addWiring('123 -> x');
        $this->addWiring('456 -> y');
        $this->addWiring('x OR y -> e');

        $this->getWireValue('e')->shouldBe(507);
    }

    function it_can_perform_a_bitwise_left_shift()
    {
        $this->addWiring('123 -> x');
        $this->addWiring('x LSHIFT 2 -> f');

        $this->getWireValue('f')->shouldBe(492);
    }

    function it_can_perform_a_bitwise_right_shift()
    {
        $this->addWiring('456 -> y');
        $this->addWiring('y RSHIFT 2 -> g');

        $this->getWireValue('g')->shouldBe(114);
    }

    function it_can_perform_a_bitwise_not()
    {
        $this->addWiring('123 -> x');
        $this->addWiring('NOT x -> h');

        $this->getWireValue('h')->shouldReturn(65412);
    }

    function it_can_perform_chained_operations()
    {
        $this->addWiring('123 -> a');
        $this->addWiring('NOT a -> b');
        $this->addWiring('NOT b -> c');

        $this->getWireValue('c')->shouldReturn(123);
    }

    function it_can_perform_nested_operations()
    {
        $this->addWiring('a OR b -> c');
        $this->addWiring('c AND d -> e');
        $this->addWiring('1 -> a');
        $this->addWiring('2 -> b');
        $this->addWiring('3 -> d');

        $this->getWireValue('e')->shouldBe(3);
    }

    function it_can_pass_the_signal_straight_from_one_wire_to_another()
    {
        $this->addWiring('123 -> b');
        $this->addWiring('b -> a');

        $this->getWireValue('a')->shouldBe(123);
    }

    function it_can_use_numbers_directly_in_the_operations()
    {
        $this->addWiring('1 AND a -> b');
        $this->addWiring('a OR 5 -> c');
        $this->addWiring('2 -> a');

        $this->getWireValue('b')->shouldBe(0);
        $this->getWireValue('c')->shouldBe(7);
    }
}
