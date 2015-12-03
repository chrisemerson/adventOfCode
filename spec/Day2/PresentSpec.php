<?php
namespace spec\AdventOfCode\Day2;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class PresentSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith("0x0x0");
    }

    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day2\Present');
    }

    function it_should_return_6_square_feet_surface_area_for_a_1ft_cube()
    {
        $this->beConstructedWith("1x1x1");
        $this->getSurfaceArea()->shouldReturn(6);
    }

    function it_should_calculate_the_surface_area_of_a_non_trivial_size()
    {
        $this->beConstructedWith("2x3x4");
        $this->getSurfaceArea()->shouldReturn(52);
    }

    function it_should_return_the_area_of_the_smallest_size_for_a_1ft_cube()
    {
        $this->beConstructedWith("1x1x1");
        $this->getAreaOfSmallestSize()->shouldReturn(1);
    }

    function it_should_return_the_area_of_the_smallest_size_for_a_non_trivial_sized_present()
    {
        $this->beConstructedWith("2x3x4");
        $this->getAreaOfSmallestSize()->shouldReturn(6);
    }
}
