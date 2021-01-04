<?php
namespace spec\AdventOfCode\Day21;

use AdventOfCode\Day21\Player;
use AdventOfCode\Day21\PlayerDecorators\Weapons\GreataxeDecorator;
use PhpSpec\ObjectBehavior;

class RPGSimSpec extends ObjectBehavior
{
    function let(Player $player, Player $opponent)
    {
        $player->getCost()->willReturn(0);
        $player->getArmor()->willReturn(0);
        $player->getDamage()->willReturn(1);

        $opponent->getCost()->willReturn(0);
        $opponent->getArmor()->willReturn(0);
        $opponent->getDamage()->willReturn(1);
    }

    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day21\RPGSim');
    }

    function it_finds_the_cheapest_way_to_win_a_game_with_no_decorators(Player $player, Player $opponent)
    {
        $player->getDamage()->willReturn(10);
        $opponent->attack(10)->shouldBeCalled();
        $opponent->isDefeated()->willReturn(true);

        $this->addPlayer('player', $player);
        $this->addPlayer('opponent', $opponent);

        $this->cheapestWin('player')->shouldReturn(0);
    }

    function it_finds_the_cheapest_way_to_win_a_game_with_1_decorator_in_1_group(Player $player, Player $opponent)
    {
        $this->beConstructedWith([[GreataxeDecorator::class]]);

        $player->getDamage()->willReturn(0);
        $opponent->attack(8)->shouldBeCalled();
        $opponent->isDefeated()->willReturn(true);

        $this->addPlayer('player', $player);
        $this->addPlayer('opponent', $opponent);

        $this->cheapestWin('player')->shouldReturn(74);
    }
}
