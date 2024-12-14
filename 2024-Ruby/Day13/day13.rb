# frozen_string_literal: true

class Day13 < AocDay
  def part1_test_answer = 480
  def part2_test_answer = super

  def part1(input) = parse_claws(input).map { |c| win_prize(c) }.sum.to_i

  def part2(input) = parse_claws(input).map { |c| {
    :A => c[:A], :B => c[:B], :prize => { :X => c[:prize][:X] + 10000000000000, :Y => c[:prize][:Y] + 10000000000000 } }
  }.map { |c| win_prize(c) }.sum.to_i

  private

  def win_prize(claw)
    a_button_gradient = claw[:A][:Y].fdiv(claw[:A][:X].to_f)
    b_button_gradient = claw[:B][:Y].fdiv(claw[:B][:X].to_f)
    b_gradient_intercept = claw[:prize][:Y] - b_button_gradient * claw[:prize][:X]

    intercept_x = b_gradient_intercept.fdiv(a_button_gradient - b_button_gradient).round

    if intercept_x % claw[:A][:X] == 0 && (claw[:prize][:X] - intercept_x) % claw[:B][:X] == 0
      3 * (intercept_x / claw[:A][:X].to_f) + (claw[:prize][:X].to_f - intercept_x) / claw[:B][:X].to_f
    else
      0
    end
  end

  def parse_claws(input) = input.strip.split("\n\n").map { |c| parse_claw(c) }

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
