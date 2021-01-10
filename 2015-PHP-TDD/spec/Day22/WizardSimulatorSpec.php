<?php
namespace spec\AdventOfCode\Day22;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class WizardSimulatorSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day22\WizardSimulator');
    }
}
