# frozen_string_literal: true

class Day11 < AocDay
  def part1_test_answer = 55312
  def part2_test_answer = nil
  def part1(input) = blink_times(parse_input(input), 25).map { |_, v| v }.sum
  def part2(input) = blink_times(parse_input(input), 75).map { |_, v| v }.sum

  private

  def parse_input(input) = input.chomp.split(" ").map(&:chomp).map(&:to_i).group_by(&:itself).transform_values!(&:size)

  def blink_times(stones, blinks) = (1..blinks)
    .reduce(stones) { |acc, _| acc
      .map { |s, c| blink(s) * c }.reduce([]) { |arr, cur| arr + cur }
      .group_by(&:itself)
      .transform_values!(&:size) }

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
