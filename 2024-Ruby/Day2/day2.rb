# frozen_string_literal: true

class Day2 < AocDay
  def part1_test_answer = 2
  def part2_test_answer = 4

  def part1(input) = input
    .chomp
    .lines
    .map(&:chomp)
    .select { |l| report_is_valid(l.split(" ").map { |i| i.chomp.to_i }) }
    .length

  def part2(input) = input
    .chomp
    .lines
    .map(&:chomp)
    .select { |l| report_is_valid_with_one_missing_level(l.split(" ").map { |i| i.chomp.to_i }) }
    .length

  private

  def report_is_valid(report)
    level_differences = report.each_cons(2).map { |a, b| a - b }

    if level_differences[0] > 0
      level_differences.reject { |i| i >= 1 && i <= 3 }.length == 0
    else
      level_differences.reject { |i| i <= -1 && i >= -3 }.length == 0
    end
  end

  def report_is_valid_with_one_missing_level(report) = report
    .each_with_index
    .map { |_, i| report.each_with_index.reject { |_, ii| i == ii }.map { |x, _| x } }
    .map { |l| report_is_valid(l) }
    .any?
end
