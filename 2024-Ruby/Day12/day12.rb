# frozen_string_literal: true

class Day12 < AocDay
  def part1_test_answer = 1930
  def part2_test_answer = 1206

  def part1(input) = find_garden_areas(parse_input(input))
    .map { |ga| ga[:points].length * find_perimeter(ga[:points]) }
    .sum

  def part2(input) = find_garden_areas(parse_input(input))
    .map { |ga| ga[:points].length * count_edges(ga[:points]) }
    .sum

  private

  def parse_input(input) = input.chomp.lines.map { |l| l.chomp.split("") }

  def find_perimeter(points) = points
    .map { |point|
      4 - [[0, 1], [0, -1], [1, 0], [-1, 0]]
        .map { |dir| [point[0] + dir[0], point[1] + dir[1]] }
        .filter { |p| points.include?(p) }
        .length
    }.sum

  def count_edges(points)
    corners = 0

    y_values = points.map { |p| p[0] }
    x_values = points.map { |p| p[1] }

    (y_values.min..y_values.max + 1).each do |y|
      (x_values.min..x_values.max + 1).each do |x|
        corner_bitmask = 0

        corner_bitmask += 1 if points.include?([y - 1, x - 1])
        corner_bitmask += 2 if points.include?([y - 1, x])
        corner_bitmask += 4 if points.include?([y, x - 1])
        corner_bitmask += 8 if points.include?([y, x])

        corners += 1 if [1, 2, 4, 7, 8, 11, 13, 14].include?(corner_bitmask)
        corners += 2 if [6, 9].include?(corner_bitmask)
      end
    end

    corners
  end

  def find_garden_areas(garden)
    garden_areas = []

    garden_cells = garden
      .each_with_index
      .reduce([]) { |acc, info| acc + info[0].each_with_index.map { |_, x| [info[1], x] } }.uniq

    unassigned_cells = garden_cells.reject { |c| garden_areas.map { |ga| ga[:points] }.flatten(1).include?(c) }.uniq

    until unassigned_cells.empty?
      cell = unassigned_cells[0]
      new_garden_area = find_garden_area(garden, cell)
      garden_areas << new_garden_area

      unassigned_cells = unassigned_cells.reject { |c| new_garden_area[:points].include?(c) }
    end

    garden_areas
  end

  def find_garden_area(garden, cell)
    cells_in_area = [cell]

    loop do
      new_cells = cells_in_area
        .map { |c| adjacent_cells(garden, c) }
        .flatten(1)
        .uniq
        .reject { |c| cells_in_area.include?(c) }
        .filter { |c| garden[c[0]][c[1]] == garden[cell[0]][cell[1]] }

      break if new_cells.empty?

      cells_in_area = cells_in_area + new_cells
    end

    { :name => garden[cell[0]][cell[1]], :points => cells_in_area }
  end

  def adjacent_cells(garden, cell) = [[0, 1], [0, -1], [1, 0], [-1, 0]]
    .map { |dir| [cell[0] + dir[0], cell[1] + dir[1]] }
    .reject { |c| c[0] < 0 || c[0] >= garden.length || c[1] < 0 || c[1] >= garden[0].length }

  def cells_adjacent(cell1, cell2) = [[0, 1], [0, -1], [1, 0], [-1, 0]]
    .map { |dir| [cell1[0] + dir[0], cell1[1] + dir[1]] }
    .map { |c| c[0] == cell2[0] && c[1] == cell2[1] }
    .any?
end
