# frozen_string_literal: true

class Day23 < AocDay
  def part1_test_answer = 7
  def part2_test_answer = 'co,de,ka,ta'

  def part1(input) = get_groups_of_3(get_connections(parse_input(input))).filter { |v| v.map { |vv| vv[0] == 't' }.any? }.length

  def part2(input)
    connections = get_connections(parse_input(input))

    groups_of_3 = get_groups_of_3(connections)

    candidate_computers = groups_of_3.flatten.uniq

    max_cliques = candidate_computers.reduce({}) { |acc, cc|
      acc.merge({ cc => grow_clique([cc], candidate_computers.reject { |ccc| ccc == cc }, connections) })
    }

    max_cliques.filter { |_, mc| mc.length == max_cliques.values.map(&:length).max }.values[0].sort.join(',')
  end

  private

  def parse_input(input) = input.strip.lines.map { |l| l.strip.split('-') }
  def get_connections(input) = input.reduce({}) { |acc, ab| add_connection(acc, ab[0], ab[1]) }.transform_values(&:uniq)

  def get_groups_of_3(connections) = connections
    .transform_values { |v| v.reduce({}) { |acc, c|
      acc[c] = connections[c]
      acc
    } }.reduce({}) { |acc, (k, v)|
    acc[k] = v.reduce({}) { |accc, (kk, vv)|
      new_vv = vv.filter { |vvv| vvv != k && connections[vvv].include?(k) }
      accc[kk] = new_vv unless new_vv.empty?
      accc
    }
    acc
  }.reduce([]) { |acc, (k, v)|
    v.each { |kk, vv| vv.each { |vvv| acc = [*acc, [k, kk, vvv].sort] } }
    acc
  }.uniq

  def grow_clique(clique, candidates, connections)
    neighbours = clique
      .map { |c| connections[c] }
      .flatten
      .filter { |nc| !clique.include?(nc) }
      .uniq
      .filter { |nc| clique.map { |cc| connections[cc].include?(nc) }.all? }.reduce(clique) { |acc, nc|
      acc.map { |accc| connections[accc].include?(nc) }.all? ? [*acc, nc] : acc
    }

    neighbours.length == clique.length ? clique : grow_clique(neighbours, candidates.reject { |c| neighbours.include?(c) }, connections)
  end

  def add_connection(hash, a, b)
    hash[a] = [] unless hash.has_key?(a)
    hash[b] = [] unless hash.has_key?(b)

    hash[a] << b
    hash[b] << a

    hash
  end
end
