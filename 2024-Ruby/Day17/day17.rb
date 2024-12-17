# frozen_string_literal: true

class Day17 < AocDay
  def part1_test_answer = "4,6,3,5,6,3,5,2,1,0"
  def part2_test_answer = nil

  def part1(input)
    initial_registers, program = parse_input(input)
    run_program(program, initial_registers)
  end

  def part2(input)
    _, program = parse_input(input)

    required_output = program.reverse

    starting_a = 1
    register_a = 1

    loop do
      register_a = starting_a

      while (program_input = required_output.delete_at(0))
        print "Looking for " + program_input.to_s + ": "

        until program_input == part2_output_from_a(register_a)
          print register_a.to_s + " => "
          print part2_output_from_a(register_a).to_s + "..."
          register_a += 1
        end

        print register_a.to_s + " => "
        print part2_output_from_a(register_a).to_s + "...Found!" + "\n\n"

        register_a *= 8
      end

      if run_program(program, { :A => register_a, :B => 0, :C => 0 }) == program.join(',')
        break
      end

      starting_a += 1
    end

    registers, program = parse_input(input)

    registers[:A] = register_a / 8

    output = run_program(program, registers)

    print "Looking for: " + program.to_s + "\n"
    print "Got:         " + output.split(',').map(&:to_i).to_s + "\n"

    registers[:A]
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
    b = a % 8
    b = b ^ 3
    c = a / (2 ** b)
    b = b ^ c
    b = b ^ 3

    b % 8
  end
end
