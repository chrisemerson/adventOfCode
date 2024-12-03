# frozen_string_literal: true

class Day3 < AocDay
  MUL_REGEX = /mul\((\d{1,3}),(\d{1,3})\)/

  def part1_test_answer = 161
  def part2_test_answer = 48

  def part1(input) = input
    .scan(MUL_REGEX)
    .map { |a, b| a.to_i * b.to_i }
    .reduce(:+)

  def part2(input) = input
    .to_enum(:scan, MUL_REGEX)
    .map { Regexp.last_match }
    .map { |x| [
      x.captures.map(&:to_i),
      get_enable_data(input)
        .filter { |_, i| i < x.offset(0)[0] }
        .last[0]
    ] }
    .filter { |_, inst| inst == 'do' }
    .map(&:first)
    .map { |a, b| a.to_i * b.to_i }
    .reduce(:+)

  private

  def get_enable_data(input) = [
    ['do', 0],
    *input
      .to_enum(:scan, /(do(?:n't)?)\(\)/)
      .map { Regexp.last_match }
      .map { |x| [x.captures[0], x.offset(0)[0]] }
      .sort_by { |_, i| i }
  ]
end
