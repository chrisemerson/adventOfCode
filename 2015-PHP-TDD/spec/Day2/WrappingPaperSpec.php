<?php
namespace spec\AdventOfCode\Day2;

use AdventOfCode\Day2\Present;
use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class WrappingPaperSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day2\WrappingPaper');
    }

    function it_should_return_0_square_feet_required_for_0_presents()
    {
        $this->getRequiredWrappingPaper()->shouldReturn(0);
    }

    function it_should_return_7_square_feet_of_wrapping_paper_required_for_a_1ft_cube_present(Present $present)
    {
        $present->getSurfaceArea()->willReturn(6);
        $present->getAreaOfSmallestSize()->willReturn(1);

        $this->addPresent($present);

        $this->getRequiredWrappingPaper()->shouldReturn(7);
    }

    function it_should_total_the_wrapping_paper_requirements_for_2_presents(Present $present1, Present $present2)
    {
        $present1->getSurfaceArea()->willReturn(52);
        $present1->getAreaOfSmallestSize()->willReturn(6);

        $present2->getSurfaceArea()->willReturn(42);
        $present2->getAreaOfSmallestSize()->willReturn(1);

        $this->addPresent($present1);
        $this->addPresent($present2);

        $this->getRequiredWrappingPaper()->shouldReturn(101);
    }

    function it_should_return_5_ft_of_ribbon_required_for_a_1ft_cube_present(Present $present)
    {
        $present->getSmallestPerimeter()->willReturn(4);
        $present->getVolume()->willReturn(1);

        $this->addPresent($present);

        $this->getRequiredRibbonLength()->shouldReturn(5);
    }

    function it_should_return_the_correct_ribbon_length_for_a_non_trivially_sized_present(Present $present)
    {
        $present->getSmallestPerimeter()->willReturn(10);
        $present->getVolume()->willReturn(24);

        $this->addPresent($present);

        $this->getRequiredRibbonLength()->shouldReturn(34);
    }

    function it_should_total_the_ribbon_requirements_for_2_presents(Present $present1, Present $present2)
    {
        $present1->getSmallestPerimeter()->willReturn(4);
        $present1->getVolume()->willReturn(10);

        $present2->getSmallestPerimeter()->willReturn(10);
        $present2->getVolume()->willReturn(24);

        $this->addPresent($present1);
        $this->addPresent($present2);

        $this->getRequiredRibbonLength()->shouldReturn(48);
    }
}
