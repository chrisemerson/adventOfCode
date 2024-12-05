# frozen_string_literal: true

class Day5 < AocDay
  def part1_test_answer = 143
  def part2_test_answer = 123

  def part1(input)
    rules, lines = parse_input(input)

    sum_middle_numbers(lines.filter { |l| check_rules(l, rules) })
  end

  def part2(input)
    rules, lines = parse_input(input)

    sum_middle_numbers(lines.reject { |l| check_rules(l, rules) }.map { |l| order_line_correctly(l, rules) })
  end

  private

  def parse_input(input)
    input_pt1, input_pt2 = input.chomp.split("\n\n")

    rules = input_pt1.lines.map(&:chomp).map { |r| r.split("|").map { |x| x.chomp.to_i } }
    lines = input_pt2.lines.map(&:chomp).map { |l| l.split(",").map { |x| x.chomp.to_i } }

    [rules, lines]
  end

  def check_rules(line, rules)
    return false unless number_is_valid_first(line[0], line, rules)
    return true if line.length == 1

    check_rules(line[1..], rules.reject { |r| r[0] == line[0] })
  end

  def sum_middle_numbers(lines) = lines.map { |l| l[(l.length - 1) / 2] }.sum

  def order_line_correctly(line, rules)
    return [] if line.empty?

    first_number = line.filter{|x| number_is_valid_first(x, line, rules)}[0]

    [first_number, *order_line_correctly(line.reject{|x| x == first_number}, rules)]
  end

  def number_is_valid_first(number, line, rules) = !rules
    .filter { |r| r[1] == number }
    .filter { |r| line.include?(r[0]) }
    .any?
end
