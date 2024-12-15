# frozen_string_literal: true

class Day11 < AocDay
  def part1_test_answer = 55312
  def part2_test_answer = 65601038650482
  def part1(input) = run_times(parse_input(input), 25)
  def part2(input) = run_times(parse_input(input), 75)

  def initialize = @cache = {}

  private

  def run_times(stones, times) = stones
    .map { |s, c| blink_times(s, times).transform_values { |v| v * c } }.reduce({}) { |acc, kv|
    kv.reduce(acc) { |iacc, ikv|
      iacc.has_key?(ikv[0]) ? iacc[ikv[0]] = iacc[ikv[0]] + ikv[1] : iacc[ikv[0]] = ikv[1]
      iacc
    }
  }.map { |_, v| v }.sum

  def parse_input(input) = input.chomp.split(" ").map(&:chomp).map(&:to_i).group_by(&:itself).transform_values!(&:size)

  def blink_times(stone, blinks)
    return { stone => 1 } if blinks.zero?

    @cache[stone] = {} unless @cache.has_key?(stone)

    unless @cache[stone].has_key?(blinks)
      test = blink(stone)
        .group_by(&:itself)
        .transform_values!(&:size).map { |s, c|
        blink_times(s, blinks - 1).transform_values { |v| v * c }
      }.reduce({}) { |acc, cur|
        cur.reduce(acc) { |acc, kv|
          acc.has_key?(kv[0]) ? acc[kv[0]] += kv[1] : acc[kv[0]] = kv[1]
          acc
        }
      }

      @cache[stone][blinks] = test
    end

    @cache[stone][blinks]
  end

  def blink(stone)
    return [1] if stone.zero?

    stone_length = Math.log10(stone).floor + 1

    if stone_length % 2 == 0
      segment_length = stone_length / 2
      first_section = (stone / (10 ** segment_length)).floor
      second_section = stone - (first_section * 10 ** segment_length)

      [first_section, second_section]
    else
      [stone * 2024]
    end
  end
end
