require "./bootstrap.rb"
require 'fileutils'

day_no = (ARGV[0] if ARGV.length > 0).to_s
part_no = ARGV[1] if ARGV.length > 1

class_loaded = false

until class_loaded
  begin
    day_class = Object.const_get("Day#{day_no}")
    day = day_class.new
    class_loaded = true
  rescue NameError, LoadError
    print "Day class does not exist: Day#{day_no}\n"

    expected_file_content = <<-eof
# frozen_string_literal: true

class Day#{day_no} < AocDay
  def part1_test_answer = super
  def part2_test_answer = super

  def part1(input)
    super
  end

  def part2(input)
    super
  end
end
eof

    print "-----------------------------\n"
    print expected_file_content
    print "-----------------------------\n"
    print "Create this file now? (y/n): "

    user_input = STDIN.gets.chomp

    print "\n"

    if user_input.downcase == 'y'
      FileUtils.mkdir_p 'Day' + day_no

      File.open("./Day#{day_no}/day#{day_no}.rb", 'w') do |f|
        f.write(expected_file_content)
      end

      File.open("./Day#{day_no}/input.txt", 'w') do |f|
        f.write('')
      end

      File.open("./Day#{day_no}/test.txt", 'w') do |f|
        f.write('')
      end
    else
      exit(1)
    end
  end
end

case part_no.to_i
when 1
  unless day.part1_test_answer.nil?
    part1_test_result = day.part1(day.get_test_input(1))

    if part1_test_result == day.part1_test_answer
      print "\e[32mPart 1 test passed\e[0m\n"

      print day.part1(day.get_input)
    else
      print "\e[31mPart 1 test failed (expected #{day.part1_test_answer}, got #{part1_test_result})\e[0m\n"
    end
  end

when 2
  unless day.part2_test_answer.nil?
    part2_test_result = day.part2(day.get_test_input(2))

    if part2_test_result == day.part2_test_answer
      print "\e[32mPart 2 test passed\e[0m\n"

      print day.part2(day.get_input)
    else
      print "\e[31mPart 2 test failed (expected #{day.part2_test_answer}, got #{part2_test_result})\e[0m\n"
    end
  end
else
  print "Unknown part: #{part_no.to_s}"
end

print "\n\n"
