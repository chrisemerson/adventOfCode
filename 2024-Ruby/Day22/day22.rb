# frozen_string_literal: true

class Day22 < AocDay
  def part1_test_answer = 37327623
  def part2_test_answer = 23

  def part1(input) = parse_input(input).map { |n| run_times(n, 2000) }.sum

  def part2(input)
    differences = parse_input(input)
      .map { |n| (1..2000).reduce([n]) { |acc, _| acc + [run_once(acc.last)] }.map { |nn| nn % 10 } }
      .map { |s| s[1..].reduce([[s[0], nil]]) { |acc, n| acc + [[n, n - acc.last[0]]] }.reject { |_, d| d.nil? } }

    differences.map { |s|
      s.each_cons(4).map { |ds| find_highest_price_for_difference_sequence(differences, ds.map { |_, d| d }) }.max
    }.max
  end

  def initialize = @cache = {}

  private

  def parse_input(input) = input.strip.lines.map(&:strip).map(&:to_i)
  def run_times(number, times) = (1..times).reduce(number) { |acc, _| run_once(acc) }

  def run_once(number)
    number = (number ^ (number * 64)) % 16777216
    number = (number ^ (number / 32)) % 16777216
    (number ^ (number * 2048)) % 16777216
  end

  def find_highest_price_for_difference_sequence(sequences, differences)
    unless @cache.has_key?(differences[0]) &&
      @cache[differences[0]].has_key?(differences[1]) &&
      @cache[differences[0]][differences[1]].has_key?(differences[2]) &&
      @cache[differences[0]][differences[1]][differences[2]].has_key?(differences[3])
      total = 0

      sequences.map do |seq|
        highest = 0

        (3..(seq.length - 1)).each do |i|
          if seq[i - 3][1] == differences[0] &&
            seq[i - 2][1] == differences[1] &&
            seq[i - 1][1] == differences[2] &&
            seq[i][1] == differences[3] &&
            seq[i][0] > highest
            highest = seq[i][0]
          end
        end

        total += highest
      end

      @cache[differences[0]] = {} unless @cache.has_key?(differences[0])
      @cache[differences[0]][differences[1]] = {} unless @cache[differences[0]].has_key?(differences[1])
      @cache[differences[0]][differences[1]][differences[2]] = {} unless @cache[differences[0]][differences[1]].has_key?(differences[2])
      @cache[differences[0]][differences[1]][differences[2]][differences[3]] = {} unless @cache[differences[0]][differences[1]][differences[2]].has_key?(differences[3])

      @cache[differences[0]][differences[1]][differences[2]][differences[3]] = total
    end

    @cache[differences[0]][differences[1]][differences[2]][differences[3]]
  end
end
