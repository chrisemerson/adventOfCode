# frozen_string_literal: true

class Day10 < AocDay
  def part1_test_answer = 36
  def part2_test_answer = super

  def part1(input)
    grid = parse_input(input)
    trailheads = find_trailheads(grid)
    routes = trailheads.map { |th| find_routes(grid, th) }

    routes.each do |trailhead|
      print "Trailhead: " + trailhead[0][0].to_s + " has " + trailhead.length.to_s + " routes\n"
      trailhead.each do |route|
        print route.to_s + "\n"
      end

      print "\n"
    end

    exit
  end

  def part2(input)
    super
  end

  private

  def parse_input(input) = input.chomp.lines.map { |l| l.chomp.split('').map(&:to_i) }

  def find_trailheads(grid) = grid
    .each_with_index
    .map { |r, y|
      r
        .each_with_index
        .filter { |v, _| v == 0 }
        .map { |_, x| [y, x] }
    }.reduce([]) { |acc, x| [*acc, *x] }

  def find_routes(grid, trailhead)
    return [[trailhead]] if grid[trailhead[0]][trailhead[1]] == 9

    routes = [[0, 1], [0, -1], [1, 0], [-1, 0]]
      .reject { |dy, dx| trailhead[0] + dy < 0 || trailhead[0] + dy >= grid.length || trailhead[1] + dx < 0 || trailhead[1] + dx >= grid[0].length }
      .map { |dy, dx| [trailhead[0] + dy, trailhead[1] + dx] }
      .filter { |y, x| grid[y][x] - grid[trailhead[0]][trailhead[1]] == 1 }
      .map{|ns| find_routes(grid, ns)}
      .reject(&:nil?)
      .map{|route| [trailhead, *route[0]]}

    routes.empty? ? nil : routes
  end
end
