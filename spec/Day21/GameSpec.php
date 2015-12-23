<?php

namespace spec\AdventOfCode\Day21;

use AdventOfCode\Day21\Player;
use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class GameSpec extends ObjectBehavior
{
    function it_is_initializable()
    {
        $this->shouldHaveType('AdventOfCode\Day21\Game');
    }

    function it_reports_the_player_as_winning(Player $player, Player $opponent)
    {
        $opponent->isDefeated()->willReturn(true);
        $opponent->attack(1)->shouldBeCalled();

        $player->isDefeated()->willReturn(false);
        $player->getDamage()->willReturn(1);

        $this->addPlayer('player', $player);
        $this->addPlayer('opponent', $opponent);

        $this->playGame()->shouldReturn('player');
    }

    function it_plays_a_move_before_declaring_the_winner(Player $player, Player $opponent)
    {
        $opponent->isDefeated()->willReturn(false);

        $opponent->attack(5)->will(
            function () {
                $this->isDefeated()->willReturn(true);
            }
        )->shouldBeCalled();

        $player->getDamage()->willReturn(5);

        $this->addPlayer('player', $player);
        $this->addPlayer('opponent', $opponent);

        $this->playGame()->shouldReturn('player');
    }
}
