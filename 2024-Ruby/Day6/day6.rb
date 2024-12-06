# frozen_string_literal: true

class Day6 < AocDay
  def part1_test_answer = 41
  def part2_test_answer = 6

  def part1(input)
    grid = parse_input(input)
    guard_position = get_guard_position(grid)
    guard_direction = [-1, 0]
    visited = Set[guard_position]

    while (0..(grid[0].length - 1)).include?(guard_position[0] + guard_direction[0]) && (0..(grid.length - 1)).include?(guard_position[1] + guard_direction[1])
      guard_position, guard_direction = take_step(grid, guard_position, guard_direction)
      visited.add(guard_position)
    end

    visited.uniq.length
  end

  def part2(input)
    grid = parse_input(input)
    grid_dump = Marshal.dump(grid)

    infinite_loops = 0

    grid.each_with_index do |row, y|
      row.each_with_index do |cell, x|
        if cell === "."
          new_grid = Marshal.load(grid_dump)
          new_grid[y][x] = "#"

          infinite_loops += 1 if infinite_loop?(new_grid)
        end
      end
    end

    infinite_loops
  end

  private

  def parse_input(input) = input.chomp.lines.map { |l| l.chomp.split('') }

  def get_guard_position(grid)
    grid.each_with_index do |row, y|
      row.each_with_index do |cell, x|
        return [y, x] if cell == "^"
      end
    end
  end

  def take_step(grid, guard_position, guard_direction)
    guard_direction = change_direction(guard_direction) while
      grid[guard_position[0] + guard_direction[0]][guard_position[1] + guard_direction[1]] === "#"

    [[guard_position[0] + guard_direction[0], guard_position[1] + guard_direction[1]], guard_direction]
  end

  def change_direction(guard_direction) = {
    -1 => { 0 => [0, 1] },
    0 => { -1 => [-1, 0], 1 => [1, 0] },
    1 => { 0 => [0, -1] } }[guard_direction[0]][guard_direction[1]]

  def infinite_loop?(grid)
    guard_position = get_guard_position(grid)
    guard_direction = [-1, 0]
    visited = Set[[*guard_position, *guard_direction]]

    while (0..(grid[0].length - 1)).include?(guard_position[0] + guard_direction[0]) && (0..(grid.length - 1)).include?(guard_position[1] + guard_direction[1])
      guard_position, guard_direction = take_step(grid, guard_position, guard_direction)

      return true if visited.include?([*guard_position, *guard_direction])

      visited.add([*guard_position, *guard_direction])
    end

    false
  end
end
