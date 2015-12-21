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

    function it_can_find_quickest_route_to_molecule_with_no_steps()
    {
        $this->setMolecule('e');

        $this->findQuickestRouteToMolecule('e')->shouldReturn(0);
    }

    function it_can_find_quickest_route_to_molecule_with_1_transformation()
    {
        $this->setMolecule('H');

        $this->addTransformation('e => H');

        $this->findQuickestRouteToMolecule('e')->shouldReturn(1);
    }

    function it_can_take_multiple_steps_to_get_to_target_molecule()
    {
        $this->setMolecule('HOH');

        $this->addTransformation('e => H');
        $this->addTransformation('e => O');
        $this->addTransformation('H => HO');
        $this->addTransformation('H => OH');
        $this->addTransformation('O => HH');

        $this->findQuickestRouteToMolecule('e')->shouldReturn(3);
    }

    function it_can_deal_with_a_longer_example()
    {
        $this->setMolecule('HOHOHO');

        $this->addTransformation('e => H');
        $this->addTransformation('e => O');
        $this->addTransformation('H => HO');
        $this->addTransformation('H => OH');
        $this->addTransformation('O => HH');

        $this->findQuickestRouteToMolecule('e')->shouldReturn(6);
    }
}
