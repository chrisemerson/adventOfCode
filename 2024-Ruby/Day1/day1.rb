# frozen_string_literal: true

class Day1 < AocDay
  def part1(input)
    left_list, right_list = get_sorted_lists(input)

    left_list.zip(right_list).map { |a, b| (a - b).abs }.reduce(:+)
  end

  def part2(input)
    left_list, right_list = get_sorted_lists(input)

    left_list.reduce(0) { |acc, l| acc + (l * (right_list.reject { |r| r != l }.length)) }
  end

  private

  def get_sorted_lists(input)
    left_list = []
    right_list = []

    input.lines.map(&:chomp)
         .map { |line| line.split(" ").map(&:chomp).map(&:to_i) }
         .each { |a, b| left_list.append(a); right_list.append(b) }

    [left_list.sort, right_list.sort]
  end
end
