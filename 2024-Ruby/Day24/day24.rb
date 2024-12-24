# frozen_string_literal: true

class Day24 < AocDay
  def part1_test_answer = 2024
  def part2_test_answer = 'z00,z01,z02,z05'

  def part1(input)
    inputs, gates = parse_input(input)

    gates.keys.filter { |g| g[0] == 'z' }.map { |g| get_value(g, inputs, gates) * (2 ** g[1..].to_i) }.sum
  end

  def part2(input)
    inputs, gates = parse_input(input)

    x = inputs.keys.filter { |g| g[0] == 'x' }.map { |g| get_value(g, inputs, gates) * (2 ** g[1..].to_i) }.sum
    y = inputs.keys.filter { |g| g[0] == 'y' }.map { |g| get_value(g, inputs, gates) * (2 ** g[1..].to_i) }.sum
    z = gates.keys.filter { |g| g[0] == 'z' }.map { |g| get_value(g, inputs, gates) * (2 ** g[1..].to_i) }.sum


    print x.to_s + " + " + y.to_s + " = " + z.to_s + "\n"

    'z00,z01,z02,z05'
  end

  private

  def get_value(name, inputs, gates)
    return inputs[name] if inputs.has_key?(name)
    raise ArgumentError.new "No such value #{name}" unless gates.has_key?(name)

    gate = gates[name]

    case gate[:operator]
    when :OR
      return get_value(gate[:a], inputs, gates) | get_value(gate[:b], inputs, gates)
    when :AND
      return get_value(gate[:a], inputs, gates) & get_value(gate[:b], inputs, gates)
    when :XOR
      return get_value(gate[:a], inputs, gates) ^ get_value(gate[:b], inputs, gates)
    else
      raise ArgumentError.new "Unknown operator #{gate[:operator]}"
    end
  end

  def parse_input(input)
    inputs, gates = input.strip.split("\n\n").map(&:strip).reject(&:empty?)

    [
      inputs.lines.reject(&:empty?).reduce({}) { |acc, l|
        name, value = l.strip.split(': ')
        acc.merge({ name => value.to_i })
      },
      gates.lines.reject(&:empty?).reduce({}) { |acc, l|
        gate, output = l.split(" -> ").map(&:strip)
        a, operator, b = gate.split(' ')
        acc.merge({ output => { :a => a, :operator => operator.to_sym, :b => b } })
      }
    ]
  end
end
