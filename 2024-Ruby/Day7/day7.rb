# frozen_string_literal: true

class Day7 < AocDay
  def part1_test_answer = 3749
  def part2_test_answer = 11387

  def part1(input) = parse_input(input)
    .filter { |e| is_equation_valid?(*e, [method(:add), method(:mul)]) }
    .map { |e| e[0] }.sum

  def part2(input) = parse_input(input)
    .filter { |e| is_equation_valid?(*e, [method(:add), method(:mul), method(:concat)]) }
    .map { |e| e[0] }.sum

  def parse_input(input) = input.chomp.lines.map do |l|
    total, numbers = l.chomp.split(":")
    [total.to_i, numbers.split(" ").map { |n| n.chomp.to_i }]
  end

  def is_equation_valid?(total, numbers, operations)
    get_possible_totals(numbers, operations)
      .map { |n| n == total }
      .any?
  end

  def get_possible_totals(numbers, operations)
    return numbers if numbers.length == 1

    *list, last = numbers

    operations
      .map { |o| get_possible_totals(list, operations).map { |t| o.call(t, last) } }
      .reduce([]) { |acc, x| [*acc, *x] }
  end

  def add (a, b) = a + b
  def mul (a, b) = a * b
  def concat (a, b) = (a.to_s + b.to_s).to_i
end
