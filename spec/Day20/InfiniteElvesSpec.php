<?php
namespace spec\AdventOfCode\Day20;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class InfiniteElvesSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day20\InfiniteElves');
    }

    function it_returns_the_number_of_presents_for_house_1()
    {
        $this->getNumberOfPresentsForHouse(1)->shouldReturn(10);
    }

    function it_returns_the_number_of_presents_for_house_2()
    {
        $this->getNumberOfPresentsForHouse(2)->shouldReturn(30);
    }

    function it_returns_the_number_of_presents_for_house_3()
    {
        $this->getNumberOfPresentsForHouse(3)->shouldReturn(40);
    }

    function it_returns_the_number_of_presents_for_higher_house_numbers()
    {
        $this->getNumberOfPresentsForHouse(4)->shouldReturn(70);
        $this->getNumberOfPresentsForHouse(5)->shouldReturn(60);
        $this->getNumberOfPresentsForHouse(6)->shouldReturn(120);
        $this->getNumberOfPresentsForHouse(7)->shouldReturn(80);
        $this->getNumberOfPresentsForHouse(8)->shouldReturn(150);
        $this->getNumberOfPresentsForHouse(9)->shouldReturn(130);
    }

    function it_returns_the_first_house_that_got_at_least_10_presents()
    {
        $this->getLowestHouseWithThisNumberOfPresents(10)->shouldReturn(1);
    }

    function it_returns_the_first_house_that_got_at_least_100_presents()
    {
        $this->getLowestHouseWithThisNumberOfPresents(100)->shouldReturn(6);
    }

    function it_ensures_elves_deliver_presents_up_to_50_houses()
    {
        $this->setElfDeliveringLimit(50);
        $this->getNumberOfPresentsForHouse(50)->shouldReturn(930);
    }

    function it_ensures_elves_stop_delivering_presents_after_50_houses()
    {
        $this->setElfDeliveringLimit(50);
        $this->getNumberOfPresentsForHouse(51)->shouldReturn(710);
    }

    function it_ensures_all_elves_stop_delivering_presents_after_50_houses()
    {
        $this->setElfDeliveringLimit(50);
        $this->getNumberOfPresentsForHouse(120)->shouldReturn(3570);
    }
}
