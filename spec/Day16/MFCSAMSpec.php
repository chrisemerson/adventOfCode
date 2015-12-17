<?php
namespace spec\AdventOfCode\Day16;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class MFCSAMSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day16\MFCSAM');
    }

    function it_finds_a_single_aunt_sue()
    {
        $this->addSue('Sue 33: vizslas: 0, cars: 2, perfumes: 1');

        $this->addDiscovery('children: 3');
        $this->addDiscovery('cats: 7');
        $this->addDiscovery('samoyeds: 2');
        $this->addDiscovery('pomeranians: 3');
        $this->addDiscovery('akitas: 0');
        $this->addDiscovery('vizslas: 0');
        $this->addDiscovery('goldfish: 5');
        $this->addDiscovery('trees: 3');
        $this->addDiscovery('cars: 2');
        $this->addDiscovery('perfumes: 1');

        $this->findSue()->shouldReturn(33);
    }

    function it_finds_aunt_sue_from_several_options()
    {
        $this->addSue('Sue 25: samoyeds: 3, trees: 8, vizslas: 5');
        $this->addSue('Sue 26: vizslas: 4, pomeranians: 2, trees: 1');
        $this->addSue('Sue 27: cars: 9, goldfish: 2, trees: 4');
        $this->addSue('Sue 28: vizslas: 6, goldfish: 10, perfumes: 7');
        $this->addSue('Sue 29: vizslas: 6, pomeranians: 3, akitas: 6');
        $this->addSue('Sue 30: trees: 0, samoyeds: 5, akitas: 9');
        $this->addSue('Sue 31: vizslas: 1, perfumes: 0, trees: 6');
        $this->addSue('Sue 32: cars: 7, vizslas: 1, children: 10');
        $this->addSue('Sue 33: vizslas: 5, cars: 1, perfumes: 7');
        $this->addSue('Sue 34: vizslas: 9, trees: 10, akitas: 9');
        $this->addSue('Sue 35: akitas: 0, vizslas: 0, cars: 2');
        $this->addSue('Sue 36: cats: 3, children: 9, samoyeds: 3');
        $this->addSue('Sue 37: vizslas: 5, pomeranians: 7, cars: 6');
        $this->addSue('Sue 38: cars: 10, akitas: 5, vizslas: 8');
        $this->addSue('Sue 39: akitas: 5, trees: 9, children: 2');
        $this->addSue('Sue 40: vizslas: 0, cats: 7, akitas: 0');
        $this->addSue('Sue 41: cars: 9, trees: 10, perfumes: 8');
        $this->addSue('Sue 42: akitas: 4, trees: 2, goldfish: 3');
        $this->addSue('Sue 43: goldfish: 1, cats: 1, akitas: 8');

        $this->addDiscovery('children: 3');
        $this->addDiscovery('cats: 7');
        $this->addDiscovery('samoyeds: 2');
        $this->addDiscovery('pomeranians: 3');
        $this->addDiscovery('akitas: 0');
        $this->addDiscovery('vizslas: 0');
        $this->addDiscovery('goldfish: 5');
        $this->addDiscovery('trees: 3');
        $this->addDiscovery('cars: 2');
        $this->addDiscovery('perfumes: 1');

        $this->findSue()->shouldReturn(35);
    }

    function it_finds_aunt_sue_from_several_options_using_test2()
    {
        $this->addSue('Sue 231: vizslas: 8, trees: 5, akitas: 9');
        $this->addSue('Sue 232: akitas: 5, goldfish: 9, trees: 1');
        $this->addSue('Sue 233: vizslas: 3, trees: 2, children: 9');
        $this->addSue('Sue 234: samoyeds: 8, perfumes: 0, cats: 0');
        $this->addSue('Sue 235: perfumes: 4, vizslas: 3, akitas: 5');
        $this->addSue('Sue 236: pomeranians: 5, vizslas: 3, akitas: 9');
        $this->addSue('Sue 237: cats: 1, trees: 7, vizslas: 5');
        $this->addSue('Sue 238: children: 5, cats: 4, samoyeds: 5');
        $this->addSue('Sue 239: trees: 3, akitas: 2, goldfish: 6');
        $this->addSue('Sue 240: goldfish: 9, trees: 1, perfumes: 1');
        $this->addSue('Sue 241: cars: 2, pomeranians: 1, samoyeds: 2');
        $this->addSue('Sue 242: akitas: 2, trees: 3, cars: 4');
        $this->addSue('Sue 243: vizslas: 6, akitas: 2, samoyeds: 7');
        $this->addSue('Sue 244: trees: 0, perfumes: 5, cars: 7');
        $this->addSue('Sue 245: goldfish: 10, perfumes: 5, vizslas: 8');
        $this->addSue('Sue 246: akitas: 0, perfumes: 0, cars: 1');
        $this->addSue('Sue 247: samoyeds: 8, goldfish: 0, cars: 6');
        $this->addSue('Sue 248: perfumes: 0, children: 10, trees: 10');
        $this->addSue('Sue 249: perfumes: 6, akitas: 5, cats: 5');
        $this->addSue('Sue 250: vizslas: 7, akitas: 4, cats: 5');
        $this->addSue('Sue 251: samoyeds: 4, akitas: 1, trees: 8');
        $this->addSue('Sue 252: perfumes: 8, pomeranians: 5, cars: 1');
        $this->addSue('Sue 253: akitas: 10, trees: 4, cats: 3');
        $this->addSue('Sue 254: perfumes: 2, cats: 2, goldfish: 9');

        $this->addDiscovery('children: 3');
        $this->addDiscovery('cats: 7');
        $this->addDiscovery('samoyeds: 2');
        $this->addDiscovery('pomeranians: 3');
        $this->addDiscovery('akitas: 0');
        $this->addDiscovery('vizslas: 0');
        $this->addDiscovery('goldfish: 5');
        $this->addDiscovery('trees: 3');
        $this->addDiscovery('cars: 2');
        $this->addDiscovery('perfumes: 1');

        $this->findSue('test2')->shouldReturn(241);
    }
}
