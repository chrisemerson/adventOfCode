# frozen_string_literal: true

class Day17 < AocDay
  def part1_test_answer = "4,6,3,5,6,3,5,2,1,0"
  def part2_test_answer = nil # Solution is specific to the algorithm in my real input, no point running on test input

  def part1(input)
    initial_registers, program = parse_input(input)
    run_program(program, initial_registers)
  end

  def part2(input)
    _, program = parse_input(input)
    required_output = program.reverse
    register_a = 1

    while (program_input = required_output.delete_at(0))
      register_a += 0

      until program_input == part2_output_from_a(register_a)
        register_a += 1
      end

      register_a <<= 3
    end

    register_a = (register_a << 3) >> 6

    until program.join(',') == run_program(program, { :A => register_a, :B => 0, :C => 0 })
      register_a += 1
    end

    register_a
  end

  private

  def parse_input(input)
    registers, program = input.strip.split("\n\n")

    [
      registers.lines.reduce({}) { |acc, l|
        parts = l.strip.split(':')
        acc.merge({ parts[0][9].to_sym => parts[1].strip.to_i })
      },
      program[9..].split(',').map { |i| i.strip.to_i }
    ]
  end

  def run_program(program, registers)
    pointer = 0
    output = []

    while pointer < program.length
      registers, pointer, output = run_program_step(registers, pointer, output, program)
    end

    output.join(',')
  end

  def run_program_step(registers, pointer, output, program)
    current_instruction = program[pointer]
    operand = program[pointer + 1]

    case current_instruction
    when 0
      registers[:A] = (registers[:A].fdiv(2 ** get_combo_operand(operand, registers))).floor
    when 1
      registers[:B] = registers[:B] ^ operand
    when 2
      registers[:B] = get_combo_operand(operand, registers) % 8
    when 3
      pointer = operand - 2 unless registers[:A].zero?
    when 4
      registers[:B] = registers[:B] ^ registers[:C]
    when 5
      output << get_combo_operand(operand, registers) % 8
    when 6
      registers[:B] = registers[:A] / 2 ** get_combo_operand(operand, registers)
    when 7
      registers[:C] = registers[:A] / 2 ** get_combo_operand(operand, registers)
    else
      raise ArgumentError.new "Invalid instruction: #{current_instruction}"
    end

    pointer += 2

    [registers, pointer, output]
  end

  def get_combo_operand(operand, registers)
    case operand
    when 0, 1, 2, 3
      operand
    when 4
      registers[:A]
    when 5
      registers[:B]
    when 6
      registers[:C]
    else
      raise ArgumentError.new "Invalid combo operand: #{operand}"
    end
  end

  def part2_output_from_a(a)
    b = a % 8 # B is last 3 digits of A
    c = a >> (b ^ 3) # C is A right shifted by a value 0 - 7
    b = b ^ c # B is XOR'd with C

    b % 8 # Last 3 bits of B output
  end
end
