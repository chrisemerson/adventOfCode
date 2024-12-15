# frozen_string_literal: true

class Day15 < AocDay
  def part1_test_answer = 10092
  def part2_test_answer = super

  def part1(input)
    room, moves = parse_input(input)

    moves.reduce(room) { |acc, move| move_robot(acc, move) }
      .each_with_index.map { |row, y| row
      .each_with_index
      .map { |cell, x|
        { :y => y, :x => x, :c => cell }
      }
    }
      .flatten(1)
      .filter { |c| c[:c] == 'O' }
      .map { |c| c[:y] * 100 + c[:x] }
      .sum
  end

  def part2(input)
    super
  end

  private

  def parse_input(input)
    room, moves = input.strip.split("\n\n")

    [room.lines.map { |l| l.strip.split("") }, moves.lines.map { |l| l.strip.split("") }.flatten]
  end

  def move_robot(room, move)
    case move
    when '^'
      dir = [-1, 0]
    when '>'
      dir = [0, 1]
    when 'v'
      dir = [1, 0]
    when '<'
      dir = [0, -1]
    else
      raise ArgumentError.new "Invalid move instruction: #{move}"
    end

    room = move_grid_piece(room, get_robot_pos(room), dir) if robot_can_move(room, dir)

    room
  end

  def robot_can_move(room, dir)
    pos = get_robot_pos(room)

    while room[pos[0]][pos[1]] != '#'
      pos = [pos[0] + dir[0], pos[1] + dir[1]]

      return true if room[pos[0]][pos[1]] == '.'
    end

    false
  end

  def move_grid_piece(room, pos, dir)
    destination_cell = room[pos[0] + dir[0]][pos[1] + dir[1]]

    return room if destination_cell == '#'

    if destination_cell != '.'
      room = move_grid_piece(room, [pos[0] + dir[0], pos[1] + dir[1]], dir)
    end

    room[pos[0] + dir[0]][pos[1] + dir[1]] = room[pos[0]][pos[1]]
    room[pos[0]][pos[1]] = '.'

    room
  end

  def get_robot_pos(room) = room
    .each_with_index
    .map { |row, y|
      row
        .each_with_index
        .map { |cell, x|
          cell == '@' ? [y, x] : nil
        }.reject(&:nil?)
    }.flatten(1)
    .reject(&:nil?)[0]

  def print_room(room)
    room.each do |line|
      print line.join('') + "\n"
    end

    print "\n"
  end
end
