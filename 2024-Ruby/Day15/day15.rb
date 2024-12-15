# frozen_string_literal: true

class Day15 < AocDay
  def part1_test_answer = 10092
  def part2_test_answer = 9021

  def part1(input)
    room, moves = parse_input(input)

    moves
      .reduce(room) { |acc, move| move_robot(acc, move, method(:move_grid_piece), method(:robot_can_move)) }
      .each_with_index.map { |row, y|
      row
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
    room, moves = parse_input(input)

    room = transform_room_for_part_2(room)

    moves.reduce(room) { |acc, move| move_robot(acc, move, method(:move_grid_piece_pt2), method(:piece_can_move)) }
      .each_with_index.map { |row, y| row
      .each_with_index
      .map { |cell, x|
        { :y => y, :x => x, :c => cell }
      }
    }
      .flatten(1)
      .filter { |c| c[:c] == '[' }
      .map { |c| c[:y] * 100 + c[:x] }
      .sum
  end

  private

  def parse_input(input)
    room, moves = input.strip.split("\n\n")

    [room.lines.map { |l| l.strip.split("") }, moves.lines.map { |l| l.strip.split("") }.flatten]
  end

  def transform_room_for_part_2(room) = room.map do |l|
    l.join('')
     .gsub('#', '##')
     .gsub('.', '..')
     .gsub('@', '@.')
     .gsub('O', '[]')
     .split('')
  end

  def move_robot(room, move, move_callback, can_move_callback)
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

    if can_move_callback.call(room, get_robot_pos(room), dir)
      room = move_callback.call(room, get_robot_pos(room), dir)
    end

    room
  end

  def robot_can_move(room, pos, dir)
    while room[pos[0]][pos[1]] != '#'
      pos = [pos[0] + dir[0], pos[1] + dir[1]]

      return true if room[pos[0]][pos[1]] == '.'
    end

    false
  end

  def piece_can_move(room, pos, dir)
    cur_piece = room[pos[0]][pos[1]]
    dest_pos = [pos[0] + dir[0], pos[1] + dir[1]]

    return true if cur_piece == '.'
    return false if cur_piece == '#'

    return piece_can_move(room, dest_pos, dir) if cur_piece == '@'
    return piece_can_move(room, dest_pos, dir) if dir == [0, 1] && %w([ ]).include?(cur_piece)
    return piece_can_move(room, dest_pos, dir) if dir == [0, -1] && %w([ ]).include?(cur_piece)

    dest_pos_arr = []

    dest_pos_arr << dest_pos
    dest_pos_arr << [dest_pos[0], dest_pos[1] + 1] if cur_piece == '['
    dest_pos_arr << [dest_pos[0], dest_pos[1] - 1] if cur_piece == ']'

    dest_pos_arr.map { |dp| piece_can_move(room, dp, dir) }.all?
  end

  def move_grid_piece(room, pos, dir)
    destination_cell = room[pos[0] + dir[0]][pos[1] + dir[1]]

    if destination_cell != '.'
      room = move_grid_piece(room, [pos[0] + dir[0], pos[1] + dir[1]], dir)
    end

    room[pos[0] + dir[0]][pos[1] + dir[1]] = room[pos[0]][pos[1]]
    room[pos[0]][pos[1]] = '.'

    room
  end

  def move_grid_piece_pt2(room, pos, dir)
    return room if room[pos[0]][pos[1]] == '.'

    object_to_move = [pos]

    if room[pos[0]][pos[1]] == ']'
      object_to_move << [pos[0], pos[1] - 1]
    elsif room[pos[0]][pos[1]] == '['
      object_to_move << [pos[0], pos[1] + 1]
    end

    if [[1, 0], [-1, 0]].include?(dir)
      destination_cells = object_to_move.map { |p| [p[0] + dir[0], p[1] + dir[1]] }
      left_behind_cells = object_to_move.map(&:itself)
    elsif dir == [0, 1]
      destination_cells = [[object_to_move[0][0], object_to_move.map { |p| p[1] }.max + 1]]
      left_behind_cells = [[object_to_move[0][0], object_to_move.map { |p| p[1] }.min]]
    else
      destination_cells = [[object_to_move[0][0], object_to_move.map { |p| p[1] }.min - 1]]
      left_behind_cells = [[object_to_move[0][0], object_to_move.map { |p| p[1] }.max]]
    end

    if destination_cells.map { |dc| room[dc[0]][dc[1]] != '.' }.any?
      room = destination_cells.reduce(room) { |acc, cur|
        move_grid_piece_pt2(acc, cur, dir)
      }
    end

    room = object_to_move.reduce(Marshal.load(Marshal.dump(room))) { |acc, cur|
      acc[cur[0] + dir[0]][cur[1] + dir[1]] = room[cur[0]][cur[1]]
      acc
    }

    left_behind_cells.reduce(Marshal.load(Marshal.dump(room))) { |acc, cur|
      acc[cur[0]][cur[1]] = '.'
      acc
    }
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
end
