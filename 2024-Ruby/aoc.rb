require "./aoc_day.rb"

autoload :Day1, "./Day1/day1.rb"
autoload :Day2, "./Day2/day2.rb"
autoload :Day3, "./Day3/day3.rb"
autoload :Day4, "./Day4/day4.rb"
autoload :Day5, "./Day5/day5.rb"
autoload :Day6, "./Day6/day6.rb"
autoload :Day7, "./Day7/day7.rb"
autoload :Day8, "./Day8/day8.rb"
autoload :Day9, "./Day9/day9.rb"
autoload :Day10, "./Day10/day10.rb"
autoload :Day11, "./Day11/day11.rb"
autoload :Day12, "./Day12/day12.rb"
autoload :Day13, "./Day13/day13.rb"
autoload :Day14, "./Day14/day14.rb"
autoload :Day15, "./Day15/day15.rb"
autoload :Day16, "./Day16/day16.rb"
autoload :Day17, "./Day17/day17.rb"
autoload :Day18, "./Day18/day18.rb"
autoload :Day19, "./Day19/day19.rb"
autoload :Day20, "./Day20/day20.rb"
autoload :Day21, "./Day21/day21.rb"
autoload :Day22, "./Day22/day22.rb"
autoload :Day23, "./Day23/day23.rb"
autoload :Day24, "./Day24/day24.rb"
autoload :Day25, "./Day25/day25.rb"

day_no = (ARGV[0] if ARGV.length > 0).to_s
part_no = ARGV[1] if ARGV.length > 1

begin
  day_class = Object.const_get("Day#{day_no}")
  day = day_class.new
rescue NameError,LoadError
  print "Day class does not exist: Day#{day_no}\n"
  print "Create a day class that extends AocDay at \'Day#{day_no}/day#{day_no}.rb\' and try again\n\n"

  print <<-eof
-----------------------------
# frozen_string_literal: true

class Day#{day_no} < AocDay
  def part1(input)
    super
  end

  def part2(input)
    super
  end
end
-----------------------------

eof
  exit(1)
end

case part_no.to_i
when 1
  unless day.part1_test_answer.nil?
    if day.part1(day.get_test_input) == day.part1_test_answer
      print "\e[32mPart 1 test passed\e[0m\n"
    else
      print "\e[31mPart 1 test failed\e[0m\n"
    end
  end

  print day.part1(day.get_input)

when 2
  unless day.part2_test_answer.nil?
    if day.part2(day.get_test_input) == day.part2_test_answer
      print "\e[32mPart 2 test passed\e[0m\n"
    else
      print "\e[31mPart 2 test failed\e[0m\n"
    end
  end

  print day.part2(day.get_input)

else
  print "Unknown part: #{part_no.to_s}"
end

print "\n\n"
