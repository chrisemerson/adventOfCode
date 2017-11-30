<?php
namespace spec\AdventOfCode\Day14;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class ReindeerRaceSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day14\ReindeerRace');
    }

    function it_can_race_a_single_reindeer()
    {
        $this->addReindeer('Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.');

        $this->getWinningReindeerDistance(1000)->shouldReturn(1056);
    }

    function it_can_race_two_reindeer()
    {
        $this->addReindeer('Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.');
        $this->addReindeer('Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.');

        $this->getWinningReindeerDistance(1000)->shouldReturn(1120);
    }

    function it_can_race_a_single_reindeer_by_points()
    {
        $this->addReindeer('Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.');

        $this->getWinningReindeerPoints(1000)->shouldReturn(1000);
    }

    function it_can_race_two_reindeer_by_points()
    {
        $this->addReindeer('Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.');
        $this->addReindeer('Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.');

        $this->getWinningReindeerPoints(1000)->shouldReturn(689);
    }
}
