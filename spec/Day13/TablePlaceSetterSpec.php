<?php
namespace spec\AdventOfCode\Day13;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class TablePlaceSetterSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day13\TablePlaceSetter');
    }

    function it_lays_a_table_for_noone()
    {
        $this->getMaximumPossibleHappinessRating()->shouldReturn(0);
    }

    function it_lays_a_table_for_two()
    {
        $this->addSeatingPreference('Alice would gain 54 happiness units by sitting next to Bob.');
        $this->addSeatingPreference('Bob would gain 83 happiness units by sitting next to Alice.');

        $this->getMaximumPossibleHappinessRating()->shouldReturn(137);
    }

    function it_can_handle_negative_happiness_units()
    {
        $this->addSeatingPreference('Alice would lose 54 happiness units by sitting next to Bob.');
        $this->addSeatingPreference('Bob would gain 83 happiness units by sitting next to Alice.');

        $this->getMaximumPossibleHappinessRating()->shouldReturn(29);
    }

    function it_can_arrange_a_table_for_three()
    {
        $this->addSeatingPreference('Alice would gain 54 happiness units by sitting next to Bob.');
        $this->addSeatingPreference('Alice would lose 79 happiness units by sitting next to Carol.');

        $this->addSeatingPreference('Bob would gain 83 happiness units by sitting next to Alice.');
        $this->addSeatingPreference('Bob would lose 7 happiness units by sitting next to Carol.');

        $this->addSeatingPreference('Carol would lose 62 happiness units by sitting next to Alice.');
        $this->addSeatingPreference('Carol would gain 60 happiness units by sitting next to Bob.');

        $this->getMaximumPossibleHappinessRating()->shouldReturn(49);
    }

    function it_can_arrange_a_table_for_four()
    {
        $this->addSeatingPreference('Alice would gain 54 happiness units by sitting next to Bob.');
        $this->addSeatingPreference('Alice would lose 79 happiness units by sitting next to Carol.');
        $this->addSeatingPreference('Alice would lose 2 happiness units by sitting next to David.');
        $this->addSeatingPreference('Bob would gain 83 happiness units by sitting next to Alice.');
        $this->addSeatingPreference('Bob would lose 7 happiness units by sitting next to Carol.');
        $this->addSeatingPreference('Bob would lose 63 happiness units by sitting next to David.');
        $this->addSeatingPreference('Carol would lose 62 happiness units by sitting next to Alice.');
        $this->addSeatingPreference('Carol would gain 60 happiness units by sitting next to Bob.');
        $this->addSeatingPreference('Carol would gain 55 happiness units by sitting next to David.');
        $this->addSeatingPreference('David would gain 46 happiness units by sitting next to Alice.');
        $this->addSeatingPreference('David would lose 7 happiness units by sitting next to Bob.');
        $this->addSeatingPreference('David would gain 41 happiness units by sitting next to Carol.');

        $this->getMaximumPossibleHappinessRating()->shouldReturn(330);
    }
}
