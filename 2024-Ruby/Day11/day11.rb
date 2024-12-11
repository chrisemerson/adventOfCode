# frozen_string_literal: true

class Day11 < AocDay
  def part1_test_answer = 55312
  def part2_test_answer = nil
  def part1(input) = blink_times(parse_input(input), 25).sum
  def part2(input) = blink_times(parse_input(input), 75).sum

  def initialize = @cache = {}

  private

  def parse_input(input) = input.chomp.split(" ").map(&:chomp).map(&:to_i)
  def blink_times(stones, blinks) = stones.map { |stone| blink_single_stone_times(stone, blinks).length }

  def blink_single_stone_times(stone, times)
    return [stone] if times.zero?

    @cache[stone] = {} unless @cache.has_key?(stone)

    unless @cache[stone].has_key?(times)
      @cache[stone][times] = blink(stone)
        .map { |s| blink_single_stone_times(s, times - 1) }
        .reduce([]) {|acc, sc| acc + sc }
    end

    @cache[stone][times]
  end

  def blink(stone)
    if stone == 0
      stones = [1]
    else
      stone_length = Math.log10(stone).floor + 1

      if stone_length % 2 == 0
        segment_length = stone_length / 2
        first_section = (stone / (10 ** segment_length)).floor
        second_section = stone - (first_section * 10 ** segment_length)
        stones = [first_section, second_section]
      else
        stones = [stone * 2024]
      end
    end

    stones
  end
end
