# frozen_string_literal: true

class Day8 < AocDay
  def part1_test_answer = 14
  def part2_test_answer = 34

  def part1(input)
    anti_nodes = Set.new

    get_antenna_locations(input).each do |_, antenna_locations|
      antenna_locations.each do |al|
        antenna_locations.filter { |oal| oal[0] != al[0] || oal[1] != al[1] }.each do |oal|
          anti_node_y = al[0] - (oal[0] - al[0])
          anti_node_x = al[1] - (oal[1] - al[1])

          anti_nodes.add([anti_node_y, anti_node_x])
        end
      end
    end

    anti_nodes
      .reject { |y, x| y < 0 || y >= input.chomp.lines.length || x < 0 || x >= input.lines[0].chomp.length }
      .uniq
      .length
  end

  def part2(input)
    anti_nodes = Set.new

    get_antenna_locations(input).each do |_, antenna_locations|
      antenna_locations.each do |al|
        antenna_locations.filter { |oal| oal[0] != al[0] || oal[1] != al[1] }.each do |oal|
          anti_node_dy = oal[0] - al[0]
          anti_node_dx = oal[1] - al[1]

          anti_node_y = al[0]
          anti_node_x = al[1]

          until anti_node_y < 0 || anti_node_x < 0 || anti_node_y >= input.chomp.lines.length || anti_node_x >= input.lines[0].chomp.length
            anti_node_y -= anti_node_dy
            anti_node_x -= anti_node_dx
          end

          anti_node_y += anti_node_dy
          anti_node_x += anti_node_dx

          until anti_node_y < 0 || anti_node_x < 0 || anti_node_y >= input.chomp.lines.length || anti_node_x >= input.lines[0].chomp.length
            anti_node_y += anti_node_dy
            anti_node_x += anti_node_dx

            anti_nodes.add([anti_node_y, anti_node_x])
          end
        end
      end
    end

    anti_nodes
      .reject { |y, x| y < 0 || y >= input.chomp.lines.length || x < 0 || x >= input.lines[0].chomp.length }
      .uniq
      .length
  end

  private

  def get_antenna_locations(input)
    input.chomp.lines.map(&:chomp).each_with_index.reduce(Hash.new) do |acc_y, row|
      line, y = row

      line.split('').each_with_index.reject { |c, _| c == '.' }.reduce(Hash.new) { |acc_x, cell|
        char, x = cell

        acc_x[char] = Set.new unless acc_x.has_key?(char)
        acc_x[char].add([y, x])

        acc_x
      }.each do |char, antenna_locations|
        acc_y[char] = Set.new unless acc_y.has_key?(char)

        antenna_locations.each do |antenna|
          acc_y[char].add(antenna)
        end
      end

      acc_y
    end
  end
end
