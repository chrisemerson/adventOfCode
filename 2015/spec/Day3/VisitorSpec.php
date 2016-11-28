<?php
namespace spec\AdventOfCode\Day3;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class VisitorSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day3\Visitor');
    }

    function it_returns_starting_position_house_visited_at_the_start()
    {
        $housesVisited = $this->getHousesVisited();

        $housesVisited->shouldBeArray();
        $housesVisited->shouldHaveCount(1);
        $housesVisited->shouldHaveKeyWithValue(0, [0 => true]);
    }

    function it_returns_2_houses_visited_after_a_single_direction()
    {
        $this->moveRight();

        $housesVisited = $this->getHousesVisited();

        $housesVisited->shouldHaveCount(2);
        $housesVisited->shouldHaveKeyWithValue(0, [0 => true]);
        $housesVisited->shouldHaveKeyWithValue(1, [0 => true]);
    }

    function it_returns_number_of_houses_visited_after_string_of_directions_that_visit_each_house_once()
    {
        $this->moveRight();
        $this->moveUp();
        $this->moveLeft();
        $this->moveUp();
        $this->moveRight();
        $this->moveRight();
        $this->moveRight();
        $this->moveDown();

        $housesVisited = $this->getHousesVisited();

        $housesVisited->shouldHaveCount(4);

        $housesVisited[0]->shouldHaveCount(3);
        $housesVisited[0]->shouldHaveKeyWithValue(0, true);
        $housesVisited[0]->shouldHaveKeyWithValue(1, true);
        $housesVisited[0]->shouldHaveKeyWithValue(2, true);

        $housesVisited[1]->shouldHaveCount(3);
        $housesVisited[1]->shouldHaveKeyWithValue(0, true);
        $housesVisited[1]->shouldHaveKeyWithValue(1, true);
        $housesVisited[1]->shouldHaveKeyWithValue(2, true);

        $housesVisited[2]->shouldHaveCount(1);
        $housesVisited[2]->shouldHaveKeyWithValue(2, true);

        $housesVisited[3]->shouldHaveCount(2);
        $housesVisited[3]->shouldHaveKeyWithValue(1, true);
        $housesVisited[3]->shouldHaveKeyWithValue(2, true);
    }

    function it_returns_number_of_houses_visited_after_revisiting_start_location()
    {
        $this->moveRight();
        $this->moveLeft();

        $housesVisited = $this->getHousesVisited();

        $housesVisited->shouldHaveCount(2);

        $housesVisited[0]->shouldHaveKeyWithValue(0, true);
        $housesVisited[1]->shouldHaveKeyWithValue(0, true);
    }

    function it_returns_number_of_houses_when_travelling_in_a_square()
    {
        $this->moveUp();
        $this->moveRight();
        $this->moveDown();
        $this->moveLeft();

        $housesVisited = $this->getHousesVisited();

        $housesVisited->shouldHaveCount(2);

        $housesVisited[0]->shouldHaveKeyWithValue(0, true);
        $housesVisited[0]->shouldHaveKeyWithValue(1, true);

        $housesVisited[1]->shouldHaveKeyWithValue(0, true);
        $housesVisited[1]->shouldHaveKeyWithValue(1, true);
    }

    function it_returns_number_of_houses_visited_after_complex_pattern()
    {
        $this->moveRight();
        $this->moveRight();
        $this->moveUp();
        $this->moveUp();
        $this->moveLeft();
        $this->moveLeft();
        $this->moveDown();
        $this->moveDown();
        $this->moveDown();
        $this->moveDown();
        $this->moveLeft();
        $this->moveLeft();
        $this->moveRight();
        $this->moveRight();
        $this->moveUp();
        $this->moveUp();

        $housesVisited = $this->getHousesVisited();

        $housesVisited->shouldHaveCount(5);

        $housesVisited[-2]->shouldHaveCount(1);
        $housesVisited[-2]->shouldHaveKeyWithValue(-2, true);

        $housesVisited[-1]->shouldHaveCount(1);
        $housesVisited[-1]->shouldHaveKeyWithValue(-2, true);

        $housesVisited[0]->shouldHaveCount(5);
        $housesVisited[0]->shouldHaveKeyWithValue(-2, true);
        $housesVisited[0]->shouldHaveKeyWithValue(-1, true);
        $housesVisited[0]->shouldHaveKeyWithValue(0, true);
        $housesVisited[0]->shouldHaveKeyWithValue(1, true);
        $housesVisited[0]->shouldHaveKeyWithValue(2, true);

        $housesVisited[1]->shouldHaveCount(2);
        $housesVisited[1]->shouldHaveKeyWithValue(0, true);
        $housesVisited[1]->shouldHaveKeyWithValue(2, true);

        $housesVisited[2]->shouldHaveCount(3);
        $housesVisited[2]->shouldHaveKeyWithValue(0, true);
        $housesVisited[2]->shouldHaveKeyWithValue(1, true);
        $housesVisited[2]->shouldHaveKeyWithValue(2, true);
    }
}
