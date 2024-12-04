# frozen_string_literal: true

class Day4 < AocDay
  def part1_test_answer = 18
  def part2_test_answer = 9
  def part1(input) = run_solution(input, method(:check_position_for_word), "XMAS")
  def part2(input) = run_solution(input, method(:check_position_for_crossed_words), "MAS")

  private

  def run_solution(input, callback, word)
    grid = get_grid(input)

    grid
      .each_with_index
      .map { |row, y|
        row
          .each_with_index
          .map { |_, x| callback.call(grid, x, y, word) }
          .reduce(:+)
      }
      .reduce(:+)
  end

  def get_grid(input) = input.chomp.lines.map { |l| l.chomp.split("") }

  def check_position_for_word(grid, x, y, word)
    [[0, -1], [1, -1], [1, 0], [1, 1], [0, 1], [-1, 1], [-1, 0], [-1, -1]]
      .filter { |dx, dy| check_direction_for_word(grid, x, y, dx, dy, word) }
      .length
  end

  def check_position_for_crossed_words(grid, x, y, word)
    [
      [[0, 0, 1, 1], [word.length - 1, 0, -1, 1]], [[0, 0, 1, 1], [0, word.length - 1, 1, -1]],
      [[0, 0, -1, -1], [-(word.length - 1), 0, 1, -1]], [[0, 0, -1, -1], [0, -(word.length - 1), -1, 1]]
    ]
      .filter { |p|
        p
          .map { |cx, cy, dx, dy|
            check_direction_for_word(grid, x + cx, y + cy, dx, dy, word) }
          .all? }
      .length
  end

  def check_direction_for_word(grid, x, y, dx, dy, word)
    return true if word.nil? || word.empty?
    return false if (y < 0 || y >= grid.length) || (x < 0 || x >= grid[0].length) || (grid[y][x] != word[0])

    check_direction_for_word(grid, x + dx, y + dy, dx, dy, word[1..])
  end
end
