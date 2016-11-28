<?php
namespace spec\AdventOfCode\Day23;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class TuringLockSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day23\TuringLock');
    }

    function it_returns_starting_values_as_0()
    {
        $this->runProgram();
        $this->getRegisterValue('a')->shouldReturn(0);
    }

    function it_increments_a_single_register()
    {
        $this->addInstruction('inc a');
        $this->runProgram();
        $this->getRegisterValue('a')->shouldReturn(1);
        $this->getRegisterValue('b')->shouldReturn(0);
    }

    function it_halves_a_registers_value()
    {
        $this->addInstruction('inc b');
        $this->addInstruction('inc b');
        $this->addInstruction('hlf b');
        $this->runProgram();
        $this->getRegisterValue('b')->shouldReturn(1);
    }

    function it_triples_a_registers_value()
    {
        $this->addInstruction('inc b');
        $this->addInstruction('tpl b');
        $this->runProgram();
        $this->getRegisterValue('b')->shouldReturn(3);
    }

    function it_jumps_2_instructions()
    {
        $this->addInstruction('jmp +2');
        $this->addInstruction('inc b');
        $this->addInstruction('inc a');
        $this->runProgram();
        $this->getRegisterValue('a')->shouldReturn(1);
        $this->getRegisterValue('b')->shouldReturn(0);
    }

    function it_breaks_out_of_the_program_at_an_undefined_instruction()
    {
        $this->addInstruction('jmp -1');
        $this->addInstruction('inc a');
        $this->runProgram();
        $this->getRegisterValue('a')->shouldReturn(0);
    }

    function it_jumps_using_jie_if_register_value_is_even()
    {
        $this->addInstruction('inc a');
        $this->addInstruction('inc a');
        $this->addInstruction('jie a, +2');
        $this->addInstruction('inc b');
        $this->addInstruction('inc a');
        $this->runProgram();
        $this->getRegisterValue('a')->shouldReturn(3);
        $this->getRegisterValue('b')->shouldReturn(0);
    }

    function it_doesnt_jump_using_jie_if_register_value_is_odd()
    {
        $this->addInstruction('inc a');
        $this->addInstruction('jie a, +2');
        $this->addInstruction('inc b');
        $this->addInstruction('inc a');
        $this->runProgram();
        $this->getRegisterValue('a')->shouldReturn(2);
        $this->getRegisterValue('b')->shouldReturn(1);
    }

    function it_jumps_using_jio_if_register_value_is_one()
    {
        $this->addInstruction('inc a');
        $this->addInstruction('jio a, +2');
        $this->addInstruction('inc b');
        $this->addInstruction('inc a');
        $this->runProgram();
        $this->getRegisterValue('a')->shouldReturn(2);
        $this->getRegisterValue('b')->shouldReturn(0);
    }

    function it_doesnt_jump_using_jio_if_register_value_is_not_one()
    {
        $this->addInstruction('inc a');
        $this->addInstruction('inc a');
        $this->addInstruction('jio a, +2');
        $this->addInstruction('inc b');
        $this->addInstruction('inc a');
        $this->runProgram();
        $this->getRegisterValue('a')->shouldReturn(3);
        $this->getRegisterValue('b')->shouldReturn(1);
    }
}
