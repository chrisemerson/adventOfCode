# frozen_string_literal: true

class Day16 < AocDay
  def part1_test_answer = 7036
  def part2_test_answer = 45

  def part1(input)
    grid = parse_input(input)

    start_pos = grid.each_with_index.map { |row, y| row.each_with_index.map { |_, x| [y, x] } }.flatten(1).filter { |y, x| grid[y][x] == 'S' }[0]
    end_pos = grid.each_with_index.map { |row, y| row.each_with_index.map { |_, x| [y, x] } }.flatten(1).filter { |y, x| grid[y][x] == 'E' }[0]
    start_dir = [0, 1]

    @min_points = {}

    explore_maze(start_pos, start_dir, 0, grid)

    @min_points[end_pos[0]][end_pos[1]].map { |_, v| v }.min
  end

  def part2(input)
    grid = parse_input(input)

    start_pos = grid.each_with_index.map { |row, y| row.each_with_index.map { |_, x| [y, x] } }.flatten(1).filter { |y, x| grid[y][x] == 'S' }[0]
    end_pos = grid.each_with_index.map { |row, y| row.each_with_index.map { |_, x| [y, x] } }.flatten(1).filter { |y, x| grid[y][x] == 'E' }[0]
    start_dir = [0, 1]

    min_points = part1(input)

    @part_of_min_path = []

    find_best_paths(start_pos, start_dir, 0, min_points, grid, [start_pos], end_pos)

    @part_of_min_path.uniq.length
  end

  def initialize
    @min_points = {}
    @part_of_min_path = []
  end

  private

  def parse_input(input) = input.strip.lines.map { |l| l.strip.split('') }

  def record_min_points(y, x, d, points)
    @min_points[y] = {} unless @min_points.has_key?(y)
    @min_points[y][x] = {} unless @min_points[y].has_key?(x)
    @min_points[y][x][{ 0 => { 1 => :E, -1 => :W }, -1 => { 0 => :N }, 1 => { 0 => :S } }[d[0]][d[1]]] = points
  end

  def get_min_points(y, x, d)
    return nil unless @min_points.has_key?(y) && @min_points[y].has_key?(x)
    @min_points[y][x][{ 0 => { 1 => :E, -1 => :W }, -1 => { 0 => :N }, 1 => { 0 => :S } }[d[0]][d[1]]]
  end

  def explore_maze(pos, dir, points, grid)
    stack = [[pos, dir, points]]

    until stack.empty?
      pos, dir, points = stack.delete_at(0)

      if get_min_points(pos[0], pos[1], dir).nil? || get_min_points(pos[0], pos[1], dir) >= points
        record_min_points(pos[0], pos[1], dir, points)

        get_adjacent_cells(pos, dir, grid).each { |ac| stack << [ac[0], ac[1], points + ac[2]] }
      end
    end
  end

  def find_best_paths(pos, dir, points, end_points, grid, visited, target)
    stack = [[pos, dir, points, visited]]

    until stack.empty?
      pos, dir, points, visited = stack.delete_at(0)

      if pos[0] == target[0] && pos[1] == target[1] && points == end_points
        @part_of_min_path += visited
        @part_of_min_path += [pos]
      end

      if get_min_points(pos[0], pos[1], dir).nil? || get_min_points(pos[0], pos[1], dir) >= points
        record_min_points(pos[0], pos[1], dir, points)

        get_adjacent_cells(pos, dir, grid).each { |ac| stack << [ac[0], ac[1], points + ac[2], visited + [ac[0]]] }
      end
    end
  end

  def get_adjacent_cells(pos, dir, grid) = [[0, 1], [0, -1], [1, 0], [-1, 0]]
    .map { |dy, dx| [pos[0] + dy, pos[1] + dx] }
    .reject { |cy, cx| cy == pos[0] - dir[0] && cx == pos[1] - dir[1] }
    .reject { |cy, cx| grid[cy][cx] == '#' }
    .map { |cy, cx| [[cy, cx], [cy - pos[0], cx - pos[1]], (cy == pos[0] + dir[0] && cx == pos[1] + dir[1] ? 1 : 1001)] }

  def print_grid(grid)
    grid.each_with_index do |row, y|
      row.each_with_index do |cell, x|
        if @part_of_min_path.include?([y, x])
          print "*"
        else
          print cell == '#' ? '█' : ' '
        end
      end

      print "\n"
    end
  end
end
