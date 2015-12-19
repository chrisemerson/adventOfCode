<?php
namespace spec\AdventOfCode\Day19;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class MedicineMachineSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day19\MedicineMachine');
    }

    function it_can_transform_a_single_character_with_a_single_transformation()
    {
        $this->setMolecule('A');

        $this->addTransformation('A => B');

        $this->getNumberOfDistinctMolecules()->shouldReturn(1);
    }

    function it_can_transform_2_characters_with_a_single_transformation()
    {
        $this->setMolecule('AA');

        $this->addTransformation('A => B');

        $this->getNumberOfDistinctMolecules()->shouldReturn(2);
    }

    function it_can_apply_multiple_transforms_to_the_same_character()
    {
        $this->setMolecule('AA');

        $this->addTransformation('A => B');
        $this->addTransformation('A => C');

        $this->getNumberOfDistinctMolecules()->shouldReturn(4);
    }

    function it_only_counts_distinct_molecules()
    {
        $this->setMolecule('HOH');

        $this->addTransformation('H => HO');
        $this->addTransformation('H => OH');
        $this->addTransformation('O => HH');

        $this->getNumberOfDistinctMolecules()->shouldReturn(4);
    }

    function it_replaces_strings_of_multiple_source_lengths()
    {
        $this->setMolecule('HOH');

        $this->addTransformation('H => H');
        $this->addTransformation('HOH => HOH');

        $this->getNumberOfDistinctMolecules()->shouldReturn(1);
    }
}
