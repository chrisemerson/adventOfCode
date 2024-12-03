# frozen_string_literal: true

class Day3 < AocDay
  def part1_test_answer = 161
  def part2_test_answer = 48

  def part1(input) = input
    .scan(/mul\((\d{1,3}),(\d{1,3})\)/)
    .map { |a, b| a.to_i * b.to_i }
    .reduce(:+)

  def part2(input)
    super
  end
end
