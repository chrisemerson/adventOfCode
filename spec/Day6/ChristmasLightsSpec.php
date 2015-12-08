<?php
namespace spec\AdventOfCode\Day6;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class ChristmasLightsSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day6\ChristmasLights');
    }

    function it_should_start_with_all_the_lights_turned_off()
    {
        $this->getNumberOfSwitchedOnLights()->shouldReturn(0);
    }

    function it_can_turn_on_a_single_light()
    {
        $this->addInstruction('turn on 0,0 through 0,0');
        $this->getNumberOfSwitchedOnLights()->shouldReturn(1);
    }

    function it_can_turn_on_two_lights_after_separate_instructions()
    {
        $this->addInstruction('turn on 0,0 through 0,0');
        $this->addInstruction('turn on 0,1 through 0,1');

        $this->getNumberOfSwitchedOnLights()->shouldReturn(2);
    }

    function it_can_turn_on_multiple_lights_with_a_single_instruction()
    {
        $this->addInstruction('turn on 0,0 through 1,1');
        $this->getNumberOfSwitchedOnLights()->shouldReturn(4);
    }

    function it_can_turn_on_a_whole_row_of_lights()
    {
        $this->addInstruction('turn on 0,0 through 999,0');
        $this->getNumberOfSwitchedOnLights()->shouldReturn(1000);
    }

    function it_can_turn_on_all_the_lights()
    {
        $this->addInstruction('turn on 0,0 through 999,999');
        $this->getNumberOfSwitchedOnLights()->shouldReturn(1000000);
    }

    function it_can_turn_off_a_single_light()
    {
        $this->addInstruction('turn on 0,0 through 0,0');
        $this->addInstruction('turn off 0,0 through 0,0');

        $this->getNumberOfSwitchedOnLights()->shouldReturn(0);
    }

    function it_can_toggle_a_light_that_is_off()
    {
        $this->addInstruction('toggle 0,0 through 0,0');
        $this->getNumberOfSwitchedOnLights()->shouldReturn(1);
    }

    function it_can_toggle_a_light_that_is_on()
    {
        $this->addInstruction('turn on 0,0 through 0,0');
        $this->addInstruction('toggle 0,0 through 0,0');

        $this->getNumberOfSwitchedOnLights()->shouldReturn(0);
    }

    function it_can_follow_a_complex_series_of_instructions()
    {
        $this->addInstruction('turn on 0,0 through 999,999');
        $this->addInstruction('turn off 2,2 through 997,997');
        $this->addInstruction('turn on 45,45 through 46,46');
        $this->addInstruction('toggle 1,1 through 4,4');

        $this->getNumberOfSwitchedOnLights()->shouldReturn(7990);
    }

    function it_should_start_with_all_the_lights_at_zero_brightness()
    {
        $this->getTotalBrightnessOfLights()->shouldReturn(0);
    }

    function it_can_increase_the_brightness_of_a_single_light()
    {
        $this->addInstruction('turn on 0,0 through 0,0');
        $this->getTotalBrightnessOfLights()->shouldReturn(1);
    }

    function it_can_increase_the_brightness_of_two_lights_after_separate_instructions()
    {
        $this->addInstruction('turn on 0,0 through 0,0');
        $this->addInstruction('turn on 0,1 through 0,1');

        $this->getTotalBrightnessOfLights()->shouldReturn(2);
    }

    function it_can_increas_the_brightness_of_multiple_lights_with_a_single_instruction()
    {
        $this->addInstruction('turn on 0,0 through 1,1');
        $this->getTotalBrightnessOfLights()->shouldReturn(4);
    }

    function it_can_increase_the_brightness_of_a_whole_row_of_lights()
    {
        $this->addInstruction('turn on 0,0 through 999,0');
        $this->getTotalBrightnessOfLights()->shouldReturn(1000);
    }

    function it_can_increase_the_brightness_of_all_the_lights()
    {
        $this->addInstruction('turn on 0,0 through 999,999');
        $this->getTotalBrightnessOfLights()->shouldReturn(1000000);
    }

    function it_can_decrease_the_brightness_of_a_single_light()
    {
        $this->addInstruction('turn on 0,0 through 0,0');
        $this->addInstruction('turn off 0,0 through 0,0');

        $this->getTotalBrightnessOfLights()->shouldReturn(0);
    }

    function it_doesnt_decrease_the_brightness_of_a_light_below_zero()
    {
        $this->addInstruction('turn off 0,0 through 0,0');
        $this->getTotalBrightnessOfLights()->shouldReturn(0);
    }

    function it_can_doubly_increase_the_brightness_of_a_light_that_is_off()
    {
        $this->addInstruction('toggle 0,0 through 0,0');
        $this->getTotalBrightnessOfLights()->shouldReturn(2);
    }

    function it_can_doubly_increase_the_brightness_of_a_light_that_is_on()
    {
        $this->addInstruction('turn on 0,0 through 0,0');
        $this->addInstruction('toggle 0,0 through 0,0');

        $this->getTotalBrightnessOfLights()->shouldReturn(3);
    }

    function it_can_follow_a_complex_series_of_instructions_and_gives_brightness()
    {
        $this->addInstruction('turn on 0,0 through 999,999');
        $this->addInstruction('turn off 2,2 through 997,997');
        $this->addInstruction('turn on 45,45 through 46,46');
        $this->addInstruction('toggle 1,1 through 4,4');

        $this->getTotalBrightnessOfLights()->shouldReturn(8020);
    }

    function it_can_increase_the_brightness_of_a_light_beyond_ten()
    {
        $this->addInstruction('toggle 0,0 through 0,0');
        $this->addInstruction('toggle 0,0 through 0,0');
        $this->addInstruction('toggle 0,0 through 0,0');
        $this->addInstruction('toggle 0,0 through 0,0');
        $this->addInstruction('toggle 0,0 through 0,0');
        $this->addInstruction('toggle 0,0 through 0,0');

        $this->getTotalBrightnessOfLights()->shouldReturn(12);
    }
}
