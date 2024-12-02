# frozen_string_literal: true

class Day2 < AocDay
  def part1_test_answer = 2
  def part2_test_answer = 4

  def part1(input) = input
    .chomp
    .lines
    .map(&:chomp)
    .select { |l| lineIsValid(l.split(" ").map { |i| i.chomp.to_i }) }
    .length

  def part2(input) = input
    .chomp
    .lines
    .map(&:chomp)
    .select { |l| lineIsValidWithOneMissingReport(l.split(" ").map { |i| i.chomp.to_i }) }
    .length

  private

  def lineIsValid(line)
    differences = line.each_cons(2).map { |a, b| a - b }

    if differences[0] > 0
      differences.reject { |i| i >= 1 && i <= 3 }.length == 0
    else
      differences.reject { |i| i <= -1 && i >= -3 }.length == 0
    end
  end

  def lineIsValidWithOneMissingReport(line) = line
    .each_with_index
    .map { |_, i| line.each_with_index.reject { |_, ii| i == ii }.map { |x, _| x } }
    .map { |l| lineIsValid(l) }
    .any?
end
