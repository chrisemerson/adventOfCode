# frozen_string_literal: true

class Day22 < AocDay
  def part1_test_answer = 37327623
  def part2_test_answer = 23

  def part1(input) = parse_input(input).map { |n| run_times(n, 2000) }.sum

  def part2(input)
    sale_indexes = parse_input(input)
      .map { |n| (1..2000).reduce([n]) { |acc, _| acc + [run_once(acc.last)] }.map { |nn| nn % 10 } }
      .map { |s| s[1..].reduce([[s[0], nil]]) { |acc, n| acc + [[n, n - acc.last[0]]] }.reject { |_, d| d.nil? } }
      .map { |s| s.each_cons(4).reduce({}) { |acc, ds|
        key = ds.map { |_, d| d }.join(',')
        acc[key] = ds[3][0] unless acc.has_key?(key)
        acc
      } }

    sale_indexes.map { |si| si.keys }.flatten.uniq.map { |as|
      sale_indexes.filter { |si| si.has_key?(as) }.map { |si| si[as] }.sum
    }.max
  end

  private

  def parse_input(input) = input.strip.lines.map(&:strip).map(&:to_i)
  def run_times(number, times) = (1..times).reduce(number) { |acc, _| run_once(acc) }

  def run_once(number)
    number = (number ^ (number * 64)) % 16777216
    number = (number ^ (number / 32)) % 16777216
    (number ^ (number * 2048)) % 16777216
  end
end
