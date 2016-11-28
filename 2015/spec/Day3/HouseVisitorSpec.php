<?php
namespace spec\AdventOfCode\Day3;

use AdventOfCode\Day3\Visitor;
use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class HouseVisitorSpec extends ObjectBehavior
{
    function let(Visitor $visitor)
    {
        $this->beConstructedWith([$visitor]);
    }

    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day3\HouseVisitor');
    }

    function it_returns_1_house_visited_when_visitor_has_been_to_1_house($visitor)
    {
        $visitor->getHousesVisited()->willReturn([0 => [0 => true]]);
        $this->getNumberOfHousesVisited()->shouldReturn(1);
    }

    function it_tells_a_visitor_to_move_for_1_direction($visitor)
    {
        $visitor->moveRight()->shouldBeCalled();

        $this->visitHouses('>');
    }

    function it_tells_a_visitor_to_move_for_3_directions($visitor)
    {
        $visitor->moveRight()->will(
            function() {
                $this->moveUp()->will(
                    function() {
                        $this->moveDown()->shouldBeCalled();
                    }
                )->shouldBeCalled();
            }
        )->shouldBeCalled();

        $this->visitHouses('>^v');
    }

    function it_tells_2_visitors_to_move_for_2_directions_each(Visitor $visitor1, Visitor $visitor2)
    {
        $this->beConstructedWith([$visitor1, $visitor2]);

        $visitor1->moveUp()->will(
            function () {
                $this->moveDown()->shouldBeCalled();
            }
        )->shouldBeCalled();

        $visitor2->moveRight()->will(
            function () {
                $this->moveLeft()->shouldBeCalled();
            }
        )->shouldBeCalled();

        $this->visitHouses('^>v<');
    }

    function it_tells_4_visitors_to_move_for_1_direction_each(
        Visitor $visitor1,
        Visitor $visitor2,
        Visitor $visitor3,
        Visitor $visitor4
    ) {
        $this->beConstructedWith([$visitor1, $visitor2, $visitor3, $visitor4]);

        $visitor1->moveUp()->shouldBeCalled();
        $visitor2->moveRight()->shouldBeCalled();
        $visitor3->moveDown()->shouldBeCalled();
        $visitor4->moveLeft()->shouldBeCalled();

        $this->visitHouses('^>v<');
    }

    function it_combines_the_visits_of_2_visitors(Visitor $visitor1, Visitor $visitor2)
    {
        $this->beConstructedWith([$visitor1, $visitor2]);

        $visitor1->getHousesVisited()->willReturn(
            [
                0 => [
                    0 => true,
                    1 => true,
                    2 => true
                ],
                1 => [
                    0 => true,
                    2 => true
                ],
                2 => [
                    0 => true,
                    1 => true,
                    2 => true
                ]
            ]
        );

        $visitor2->getHousesVisited()->willReturn(
            [
                -2 => [
                    -2 => true
                ],
                -1 => [
                    -2 => true
                ],
                0 => [
                    -2 => true,
                    -1 => true,
                    0 => true
                ],
            ]
        );

        $this->getNumberOfHousesVisited()->shouldReturn(12);
    }
}
