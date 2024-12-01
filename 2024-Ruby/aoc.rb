require "./aoc_day.rb"

autoload :Day1, "./Day1/day1.rb"

day = ARGV[0] if ARGV.length > 0
part = ARGV[1] if ARGV.length > 1

begin
  day_class = Object.const_get('Day' + day.to_s)
  day = day_class.new

  case part.to_i
  when 1
    print day.part1(day.getInput)

  when 2
    print day.part2(day.getInput)

  else
    print "Unknown part: " + part.to_s
  end
rescue NameError
  print "Day class does not exist: Day" + day + "\n"
  print "Create a day class that extends AocDay at \'Day" + day + "/Day" + day + ".rb\' and try again\n\n"

  print <<-eof
-----------------------------
# frozen_string_literal: true

class Day#{day} < AocDay
  def part1(input)
    super
  end

  def part2(input)
    super
  end
end
-----------------------------
eof
end

print "\n\n"
