# frozen_string_literal: true

class Day24 < AocDay
  def part1_test_answer = 2024
  def part2_test_answer = nil

  def part1(input)
    inputs, gates = parse_input(input)

    gates.keys.filter { |g| g[0] == 'z' }.map { |g| get_value(g, inputs, gates) * (2 ** g[1..].to_i) }.sum
  end

  def part2(input)
    inputs, gates = parse_input(input)

    num_len = inputs.keys.filter { |g| g[0] == 'x' }.length

    swaps = [%w[z09 gwh], %w[wgb wbw], %w[z21 rcb], %w[jct z39]]
    ignore = ['vvc']
    start_at = 22

    swaps.each do |(x, y)|
      temp = gates[x]
      gates[x] = gates[y]
      gates[y] = temp
    end

    (start_at..num_len).each do |i|
      x = 2 ** i
      y = 2 ** i

      inputs = x.to_s(2).rjust(num_len, '0').reverse.split('').each_with_index
                .reduce({}) { |acc, (c, i)| acc.merge({ 'x' + i.to_s.rjust(2, '0') => c.to_i }) }

      inputs = y.to_s(2).rjust(num_len, '0').reverse.split('').each_with_index
                .reduce(inputs) { |acc, (c, i)| acc.merge({ 'y' + i.to_s.rjust(2, '0') => c.to_i }) }

      begin
        x = inputs.keys.filter { |g| g[0] == 'x' }.map { |g| get_value(g, inputs, gates) * (2 ** g[1..].to_i) }.sum
        y = inputs.keys.filter { |g| g[0] == 'y' }.map { |g| get_value(g, inputs, gates) * (2 ** g[1..].to_i) }.sum
        z = gates.keys.filter { |g| g[0] == 'z' }.map { |g| get_value(g, inputs, gates) * (2 ** g[1..].to_i) }.sum
      rescue ArgumentError
        next
      end

      print x.to_s + " + " + y.to_s + " = " + z.to_s + "\n"

      unless x + y == z
        begin
          get_value('z' + i.to_s.rjust(2, '0'), inputs, gates, true, '', ignore).to_s
          gates_to_ignore = STDIN.gets
          ignore = [*ignore, *gates_to_ignore.split(',').map(&:strip)]
        rescue ArgumentError
          next
        end
      end
    end

    swaps.flatten.uniq.sort.join(',')
  end

  private

  def get_value(name, inputs, gates, trace = false, indent = "", ignore = [])
    if ignore.include?(name)
      trace = false
    end

    return inputs[name] if inputs.has_key?(name)

    raise ArgumentError.new "No such value #{name}" unless gates.has_key?(name)

    gate = gates[name]

    if trace
      print indent + name + ": " + gate[:a] + " " + gate[:operator].to_s + " " + gate[:b] + "\n"
    end

    case gate[:operator]
    when :OR
      get_value(gate[:a], inputs, gates, trace, indent + "  ", ignore) | get_value(gate[:b], inputs, gates, trace, indent + "  ", ignore)
    when :AND
      get_value(gate[:a], inputs, gates, trace, indent + "  ", ignore) & get_value(gate[:b], inputs, gates, trace, indent + "  ", ignore)
    when :XOR
      get_value(gate[:a], inputs, gates, trace, indent + "  ", ignore) ^ get_value(gate[:b], inputs, gates, trace, indent + "  ", ignore)
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
