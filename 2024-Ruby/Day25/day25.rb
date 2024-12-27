# frozen_string_literal: true

class Day25 < AocDay
  def part1_test_answer = 3

  def part1(input)
    locks, keys = get_locks_and_keys(input)

    locks.map { |lock|
      keys.map { |key|
        (0..lock.length - 1).filter { |i| lock[i] + key[i] > 5 }.length > 0 ? 0 : 1
      }.sum
    }.sum
  end

  private

  def get_locks_and_keys(input)
    lk = input.strip.split("\n\n").map { |g| g.strip.split("\n").map { |l| l.strip.split("") } }

    [
      lk
        .filter { |lok| lok[0].filter { |c| c == '#' }.length == 5 }
        .map { |lock| lock.transpose.map { |lk| lk.filter { |lkc| lkc == '#' }.length - 1 } },
      lk
        .filter { |lok| lok[0].filter { |c| c == '.' }.length == 5 }
        .map { |key| key.transpose.map(&:reverse).map { |lk| lk.filter { |lkc| lkc == '#' }.length - 1 } }
    ]
  end
end
