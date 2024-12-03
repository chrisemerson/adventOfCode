# frozen_string_literal: true

class AocDay
  def get_input = File.read('./' + self.class.name.to_s + '/input.txt')

  def get_test_input(part_no) =
    if File.exist?('./' + self.class.name.to_s + "/test_pt#{part_no}.txt")
      File.read('./' + self.class.name.to_s + "/test_pt#{part_no}.txt")
    else
      File.read('./' + self.class.name.to_s + "/test.txt")
    end

  def part1(input) = print "Part 1 not yet implemented"
  def part1_test_answer = print "Part 1 test solution not provided\n"
  def part2(input) = print "Part 2 not yet implemented"
  def part2_test_answer = print "Part 2 test solution not provided\n"
end
