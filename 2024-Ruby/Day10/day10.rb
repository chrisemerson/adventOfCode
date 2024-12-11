# frozen_string_literal: true

class Day10 < AocDay
  def part1_test_answer = 36
  def part2_test_answer = 81

  def part1(input) = find_trailheads(parse_input(input))
    .map { |th| find_routes(parse_input(input), th) }
    .map { |route| route.map(&:last).uniq.length }
    .sum

  def part2(input) = find_trailheads(parse_input(input))
    .map { |th| find_routes(parse_input(input), th) }
    .map(&:length)
    .sum

  private

  def parse_input(input) = input.chomp.lines.map { |l| l.chomp.split('').map(&:to_i) }

  def find_trailheads(grid) = grid
    .each_with_index
    .map { |r, y| r.each_with_index.filter { |v, _| v == 0 }.map { |_, x| [y, x] }
    }.reduce([]) { |acc, x| [*acc, *x] }

  def find_routes(grid, trailhead)
    return [[trailhead]] if grid[trailhead[0]][trailhead[1]] == 9

    routes_from_here = find_cells_to_jump_to(grid, trailhead)
      .map { |c| find_routes(grid, c) }
      .reject(&:nil?)
      .reduce([]) { |acc, rs| acc + rs.map { |r| [trailhead, *r] } }

    routes_from_here.empty? ? nil : routes_from_here
  end

  def find_cells_to_jump_to(grid, cell) = [[0, 1], [0, -1], [1, 0], [-1, 0]]
    .reject { |dy, dx| cell[0] + dy < 0 || cell[0] + dy >= grid.length || cell[1] + dx < 0 || cell[1] + dx >= grid[0].length }
    .map { |dy, dx| [cell[0] + dy, cell[1] + dx] }
    .filter { |y, x| grid[y][x] - grid[cell[0]][cell[1]] == 1 }
end
