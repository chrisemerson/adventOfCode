# frozen_string_literal: true

class Day20 < AocDay
  def part1_test_answer = nil
  def part2_test_answer = super

  def part1(input)
    grid = parse_input(input)

    base_path_length = find_path(grid)

    grid
      .each_with_index
      .map { |row, y| row
        .each_with_index
        .map { |_, x| is_shortcut_position(grid, y, x) ? find_path(grid, [y, x]) : nil }
        .reject(&:nil?) }
      .reduce([]) { |acc, x| acc + x }
      .map { |bp| base_path_length - bp }
      .filter { |cs| cs >= 100 }
      .length
  end

  def part2(input)
    super
  end

  def initialize = @min_points = {}

  private

  def parse_input(input) = input.strip.lines.map { |line| line.strip.split('') }

  def find_path(grid, invisible_wall = nil)
    @min_points = {}

    start_pos = grid
      .each_with_index
      .map { |row, y| row
        .each_with_index
        .map { |cell, x| [cell, [y, x]] } }
      .reduce([]) { |acc, row| acc + row }
      .filter { |d| d[0] == "S" }[0][1]

    end_pos = grid
      .each_with_index
      .map { |row, y| row
        .each_with_index
        .map { |cell, x| [cell, [y, x]] } }
      .reduce([]) { |acc, row| acc + row }
      .filter { |d| d[0] == "E" }[0][1]

    find_best_path(start_pos, 0, grid, invisible_wall)

    get_min_points(*end_pos)
  end

  def record_min_points(y, x, points)
    @min_points[y] = {} unless @min_points.has_key?(y)
    @min_points[y][x] = points
  end

  def get_min_points(y, x)
    return nil unless @min_points.has_key?(y) && @min_points[y].has_key?(x)
    @min_points[y][x]
  end

  def find_best_path(pos, points, grid, invisible_wall)
    stack = [[pos, points]]

    until stack.empty?
      pos, points = stack.delete_at(0)

      if get_min_points(pos[0], pos[1]).nil? || get_min_points(pos[0], pos[1]) >= points
        record_min_points(pos[0], pos[1], points)

        get_adjacent_cells(pos, grid, invisible_wall).each { |ac| stack << [ac, points + 1] }
      end
    end
  end

  def get_adjacent_cells(pos, grid, invisible_wall) = [[0, 1], [1, 0], [0, -1], [-1, 0]]
    .map { |dy, dx| [pos[0] + dy, pos[1] + dx] }
    .reject { |cy, cx| cy < 0 || cy >= grid.length || cx < 0 || cx >= grid[0].length }
    .filter { |cy, cx| grid[cy][cx] != '#' || (!invisible_wall.nil? && cy == invisible_wall[0] && cx == invisible_wall[1]) }
    .sort_by { |cy, cx| (grid.length - cy) + (grid[0].length - cx) }

  def is_shortcut_position(grid, y, x) = grid[y][x] == '#' &&
    y > 0 &&
    y < grid.length - 1 &&
    x > 0 &&
    x < grid.length - 1 &&
    (
      (%w[. S E].include?(grid[y - 1][x]) && %w[. S E].include?(grid[y + 1][x])) ||
        (%w[. S E].include?(grid[y][x - 1]) && %w[. S E].include?(grid[y][x + 1]))
    )

end
