# frozen_string_literal: true

class Day12 < AocDay
  def part1_test_answer = 1930

  def part2_test_answer = super

  def part1(input) = find_garden_areas(parse_input(input))
    .map { |ga| ga[:points].length * find_perimeter(ga[:points]) }
    .sum

  def part2(input) = super

  private

  def parse_input(input) = input.chomp.lines.map { |l| l.chomp.split("") }

  def find_perimeter(points) = points
    .map { |point|
      4 - [[0, 1], [0, -1], [1, 0], [-1, 0]]
        .map { |dir| [point[0] + dir[0], point[1] + dir[1]] }
        .filter { |p| points.include?(p) }
        .length
    }.sum

  def find_garden_areas(garden)
    garden_areas = []

    garden_cells = garden
      .each_with_index
      .reduce([]) { |acc, info| acc + info[0].each_with_index.map { |_, x| [info[1], x] } }

    loop do
      unassigned_cells = garden_cells.reject { |c| cell_in_garden_areas(garden_areas, c) }

      break if unassigned_cells.empty?

      cell = unassigned_cells[0]
      garden_areas << find_garden_area(garden, cell)
    end

    garden_areas
  end

  def find_garden_area(garden, cell)
    cells_in_area = [cell]
    cell_name = garden[cell[0]][cell[1]]
    cell_assigned = true

    unassigned_cells_of_same_letter = garden
      .each_with_index
      .map { |row, y| row.each_with_index.map { |_, x| [y, x] } }
      .reduce([]) { |acc, row| acc + row.filter { |c| garden[c[0]][c[1]] == cell_name } }

    while cell_assigned
      cell_assigned = false

      adjacent_cells = unassigned_cells_of_same_letter
        .filter { |c| cells_in_area.filter { |cc| cc[0] == c[0] && cc[1] == c[1] }.empty? }
        .filter { |c| cells_in_area.map { |cia| cells_adjacent(cia, c) }.any? }

      unless adjacent_cells.empty?
        cells_in_area += adjacent_cells
        cell_assigned = true
      end
    end

    { :name => garden[cell[0]][cell[1]], :points => cells_in_area.uniq }
  end

  def cell_in_garden_areas(garden_areas, cell) = garden_areas
    .map { |ga| ga[:points].map { |p| p[0] == cell[0] && p[1] == cell[1] }.any? }
    .any?

  def cells_adjacent(cell1, cell2)
    [[0, 1], [0, -1], [1, 0], [-1, 0]]
    .map { |dir| [cell1[0] + dir[0], cell1[1] + dir[1]] }
    .map { |c| c[0] == cell2[0] && c[1] == cell2[1] }
    .any?
    end
end
