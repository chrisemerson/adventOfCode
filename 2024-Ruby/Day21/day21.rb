# frozen_string_literal: true

class Day21 < AocDay
  def part1_test_answer = 126384
  def part2_test_answer = 154115708116294
  def part1(input) = type_code_with_intermediate_robots(input, 2)
  def part2(input) = type_code_with_intermediate_robots(input, 25)

  def initialize = @cache = {}

  private

  def type_code_with_intermediate_robots(input, intermediate_robots) = input
    .lines
    .map(&:strip)
    .map { |c| [c, find_keypad_sequence(c, numeric_keypad_layout)] }
    .map { |orig_code, s|
      orig_code[0..-1].to_i * ('A' + s)
        .split('')
        .each_cons(2)
        .map { |p, q| get_key_presses(p, q, intermediate_robots) }
        .sum
    }.sum

  def get_key_presses(from, to, depth)
    unless @cache.has_key?(from) && @cache[from].has_key?(to) && @cache[from][to].has_key?(depth)
      @cache[from] = {} unless @cache.has_key?(from)
      @cache[from][to] = {} unless @cache[from].has_key?(to)

      code = find_keypad_sequence(from + to, arrow_keypad_layout)
      code = code[code.index('A') + 1..]

      if depth == 1
        button_presses = code.length
      else
        button_presses = ('A' + code).split('').each_cons(2).map { |p, q| get_key_presses(p, q, depth - 1) }.sum
      end

      @cache[from][to][depth] = button_presses
    end

    @cache[from][to][depth]
  end

  def find_keypad_sequence(code, keypad)
    current_pos = [0, 0]
    button_seq = ''

    code.split('').each do |cc|
      target_pos = keypad[cc]

      dy = target_pos[0] - current_pos[0]
      dx = target_pos[1] - current_pos[1]

      button_seq_x = dx > 0 ? ('>' * dx) : ('<' * (0 - dx))
      button_seq_y = dy > 0 ? ('v' * dy) : ('^' * (0 - dy))

      if (current_pos == [0, 0] && button_seq_x == "<<") || (current_pos == [0, -1] && button_seq_x == "<")
        button_seq += button_seq_y + button_seq_x + 'A'
      elsif (current_pos == [-3, -2] && button_seq_y == "vvv") || (current_pos == [-2, -2] && button_seq_y == "vv") ||
        (current_pos == [-1, -2] && button_seq_y == "v") || (current_pos == [1, -2] && button_seq_y == "^")
        button_seq += button_seq_x + button_seq_y + 'A'
      elsif button_seq_x[0] == '<'
        button_seq += button_seq_x + button_seq_y + 'A'
      else
        button_seq += button_seq_y + button_seq_x + 'A'
      end

      current_pos = target_pos
    end

    button_seq
  end

  def numeric_keypad_layout = {
    '7' => [-3, -2], '8' => [-3, -1], '9' => [-3, 0],
    '4' => [-2, -2], '5' => [-2, -1], '6' => [-2, 0],
    '1' => [-1, -2], '2' => [-1, -1], '3' => [-1, 0],
                     '0' => [0, -1],  'A' => [0, 0]
  }

  def arrow_keypad_layout = {
                    '^' => [0, -1], 'A' => [0, 0],
    '<' => [1, -2], 'v' => [1, -1], '>' => [1, 0]
  }
end
