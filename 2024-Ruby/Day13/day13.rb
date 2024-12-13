# frozen_string_literal: true

class Day13 < AocDay
  def part1_test_answer = 480

  def part2_test_answer = super

  def part1(input)
    claws = parse_claws(input)

    claws.map { |c| win_prize(c) }

  end

  def part2(input)
    parse_claws(input)
  end

  private

  def win_prize(claw)
    try_buttons(claw[:A][:X], claw[:A][:Y], claw[:B][:X], claw[:B][:Y], claw[:prize][:X], claw[:prize][:Y])
  end

  def try_buttons(ax, ay, bx, by, px, py, x = 0, y = 0)
    return 0 if x == px && y == py
    return nil if x > px || y > py

    options = [
      try_buttons(ax, ay, bx, by, px, py, x + ax, y + ay),
      try_buttons(ax, ay, bx, by, px, py, x + bx, y + by)
    ].reject(&:nil?)

    options.empty? ? nil : options.min
  end

  def parse_claws(input)
    claws = input.strip.split("\n\n").map { |c| parse_claw(c) }
  end

  def parse_claw(claw)
    claw_details = {}

    claw.strip.split("\n").map do |line|
      details = line.split(":")
      coords_details = details[1].split(",").map(&:strip)

      case details[0]
      when "Button A"
        key = :A
        coords = parse_coords(coords_details)
      when "Button B"
        key = :B
        coords = parse_coords(coords_details)
      when "Prize"
        key = :prize
        coords = parse_coords(coords_details)
      end

      claw_details[key] = coords
    end

    claw_details
  end

  def parse_coords(coords)
    coords_hash = {}

    coords.map do |coord|
      case coord[0]
      when 'X'
        coords_hash[:X] = coord[2..].to_i
      when 'Y'
        coords_hash[:Y] = coord[2..].to_i
      end
    end

    coords_hash
  end
end
