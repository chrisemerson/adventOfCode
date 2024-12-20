# frozen_string_literal: true

class Day20 < AocDay
  def part1_test_answer = nil
  def part2_test_answer = nil

  def part1(input)
    find_path(parse_input(input))

    find_cheats(2).filter { |cs| cs >= 100 }.length
  end

  def part2(input)
    find_path(parse_input(input))

    find_cheats(20).filter { |cs| cs >= 100 }.length
  end

  def initialize = @min_points = {}

  private

  def parse_input(input) = input.strip.lines.map { |line| line.strip.split('') }

  def find_path(grid)
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

    find_best_path(start_pos, 0, grid)

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

  def find_best_path(pos, points, grid)
    stack = [[pos, points]]

    until stack.empty?
      pos, points = stack.delete_at(0)

      if get_min_points(pos[0], pos[1]).nil? || get_min_points(pos[0], pos[1]) >= points
        record_min_points(pos[0], pos[1], points)

        get_adjacent_cells(pos, grid).each { |ac| stack << [ac, points + 1] }
      end
    end
  end

  def get_adjacent_cells(pos, grid) = [[0, 1], [1, 0], [0, -1], [-1, 0]]
    .map { |dy, dx| [pos[0] + dy, pos[1] + dx] }
    .reject { |cy, cx| cy < 0 || cy >= grid.length || cx < 0 || cx >= grid[0].length }
    .reject { |cy, cx| grid[cy][cx] == '#' }
    .sort_by { |cy, cx| (grid.length - cy) + (grid[0].length - cx) }

  def find_cheats(cheat_time)
    points_on_path = @min_points.map { |y, xs| xs.map { |x, p| [[y, x], p] } }.reduce([]) { |acc, x| acc + x }

    points_on_path.map { |coords, points|
      points_on_path
        .map { |icoords, ipoints|
          manhattan_distance = (icoords[0] - coords[0]).abs + (icoords[1] - coords[1]).abs

          manhattan_distance <= cheat_time && ipoints > points + manhattan_distance ? ipoints - points - manhattan_distance : nil }
        .reject(&:nil?)
    }.reduce([]) { |acc, x| acc + x }
  end
end
