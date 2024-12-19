# frozen_string_literal: true

class Day19 < AocDay
  def part1_test_answer = 6
  def part2_test_answer = 16

  def part1(input)
    @designs_possible = {}

    patterns, designs = parse_input(input)

    designs.map { |d| design_combinations_possible(d, patterns) }.reject { |dc| dc.zero? }.length
  end

  def part2(input)
    @designs_possible = {}

    patterns, designs = parse_input(input)

    designs.map { |d| design_combinations_possible(d, patterns) }.sum
  end

  def initialize = @designs_possible = {}

  private

  def parse_input(input)
    patterns, designs = input.strip.split("\n\n")

    [patterns.split(',').map(&:strip), designs.lines.map(&:strip)]
  end

  def design_combinations_possible(design, patterns)
    unless @designs_possible.has_key?(design)
      if design.length == 0
        @designs_possible[design] = 1
      else
        @designs_possible[design] = patterns
          .filter { |p| design[0..p.length - 1] == p }
          .map { |p| design_combinations_possible(design[p.length..], patterns) }.sum
      end
    end

    @designs_possible[design]
  end
end
