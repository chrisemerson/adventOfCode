# frozen_string_literal: true

class Day18 < AocDay
  def part1_test_answer = 22
  def part2_test_answer = "6,1"

  def part1(input)
    @min_points = {}

    grid, _ = parse_input(input)

    explore_grid(grid, [0, 0], 0)

    @min_points[grid.length - 1][grid[0].length - 1]
  end

  def part2(input)
    grid, other_bytes = parse_input(input)

    while (other_byte = other_bytes.delete_at(0))
      grid[other_byte[0]][other_byte[1]] = '#'

      @min_points = {}
      explore_grid(grid, [0, 0], 0)

      if get_min_points(grid.length - 1, grid[0].length - 1).nil?
        return other_byte[1].to_s + "," + other_byte[0].to_s
      end
    end

    "All bytes added"
  end

  def initialize = @min_points = {}

  private

  def parse_input(input)
    grid_info, bytes = input.strip.split("\n\n")
    bytes = bytes.strip.lines.map { |b| b.strip.split(',').reverse.map(&:to_i) }

    grid_size, first_n = grid_info.split(',').map { |gi| gi.strip.to_i }

    grid = []

    (0..grid_size.to_i).each do |y|
      grid[y] = []

      (0..grid_size.to_i).each do |x|
        grid[y][x] = bytes[0..(first_n - 1)].include?([y, x]) ? '#' : '.'
      end
    end

    [grid, bytes[first_n..]]
  end

  def record_min_points(y, x, points)
    @min_points[y] = {} unless @min_points.has_key?(y)
    @min_points[y][x] = points
  end

  def get_min_points(y, x)
    return nil unless @min_points.has_key?(y) && @min_points[y].has_key?(x)
    @min_points[y][x]
  end

  def explore_grid(grid, pos, points)
    queue = [[pos, points]]

    until queue.empty?
      pos, points = queue.delete_at(0)

      if get_min_points(pos[0], pos[1]).nil?
        record_min_points(pos[0], pos[1], points)

        get_adjacent_cells(pos, grid).each { |ac| queue << [ac, points + 1] }
      end
    end
  end

  def get_adjacent_cells(pos, grid) = [[0, 1], [1, 0], [0, -1], [-1, 0]]
    .map { |dy, dx| [pos[0] + dy, pos[1] + dx] }
    .reject { |cy, cx| cy < 0 || cy >= grid.length || cx < 0 || cx >= grid[0].length }
    .reject { |cy, cx| grid[cy][cx] == '#' }
    .sort_by { |cy, cx| (grid.length - cy) + (grid[0].length - cx) }
end
