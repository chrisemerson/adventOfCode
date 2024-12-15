# frozen_string_literal: true

class Day14 < AocDay
  # def grid_width = 11
  # def grid_height = 7
  # def part1_test_answer = 12

  def grid_width = 101
  def grid_height = 103
  def part1_test_answer = nil
  def part2_test_answer = super

  def part1(input)
    robots = (1..100).reduce(parse_input(input)) { |acc, _| move_robots(acc) }.map { |r| r[:position] }

    half_grid_width = (grid_width - 1) / 2
    half_grid_height = (grid_height - 1) / 2

    [
      robots.filter { |r| r[:X] < half_grid_width && r[:Y] < half_grid_height }.length,
      robots.filter { |r| r[:X] > half_grid_width && r[:Y] < half_grid_height }.length,
      robots.filter { |r| r[:X] < half_grid_width && r[:Y] > half_grid_height }.length,
      robots.filter { |r| r[:X] > half_grid_width && r[:Y] > half_grid_height }.length
    ].reduce(1) { |acc, c| acc * c }
  end

  def part2(input)
    robots = parse_input(input)
    count = 0

    possible_tree = false

    until possible_tree
      count += 1
      robots = move_robots(robots)

      (0..grid_height).each do |y|
        robots_in_a_row = 0

        (0..grid_width).each do |x|
          if robots.filter { |r| r[:position][:Y] == y && r[:position][:X] == x }.length > 0
            robots_in_a_row += 1

            if robots_in_a_row >= 10
              possible_tree = true
            end
          else
            robots_in_a_row = 0
          end
        end
      end
    end

    print_robots(robots.map { |r| r[:position] }, count)
  end

  private

  def parse_input(input)
    input.strip.lines.map { |l| parse_line(l.strip) }
  end

  def parse_line(line)
    line_parts = line.split(" ")
    position_parts, velocity_parts = line_parts.map { |lp| lp.strip.split('=') }

    pos_x, pos_y = position_parts[1].split(',')
    v_x, v_y = velocity_parts[1].split(',')

    { :position => { :X => pos_x.to_i, :Y => pos_y.to_i }, :velocity => { :X => v_x.to_i, :Y => v_y.to_i } }
  end

  def move_robots(robots)
    robots.map do |r|
      {
        :velocity => r[:velocity],
        :position => {
          :X => (r[:position][:X] + r[:velocity][:X]) % grid_width,
          :Y => (r[:position][:Y] + r[:velocity][:Y]) % grid_height
        }
      }
    end
  end

  def print_robots(robots, count)
    print "After " + count.to_s + " seconds, the robots look like this:\n"

    (1..grid_height).each do |y|
      (1..grid_width).each do |x|
        if robots.filter { |r| r[:Y] == y && r[:X] == x }.length > 0
          print '#'
        else
          print '.'
        end
      end
      print "\n"
    end
  end
end
