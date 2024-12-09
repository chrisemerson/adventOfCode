# frozen_string_literal: true

class Day9 < AocDay
  def part1_test_answer = 1928
  def part2_test_answer = 2858

  def part1(input)
    disk = get_disk(input)

    start_pos = 0
    end_pos = disk.length - 1
    checksum = 0

    while start_pos <= end_pos
      if disk[start_pos] == '.'
        while disk[end_pos] == '.'
          end_pos -= 1
        end

        checksum += (start_pos * disk[end_pos]) if end_pos > start_pos
        end_pos -= 1
      else
        checksum += (start_pos * disk[start_pos])
      end

      start_pos += 1
    end

    checksum
  end

  def part2(input)
    disk_layout = get_disk_layout(input)
    file_id = disk_layout.each_with_index.reject { |info, _| info[0] == '.' }.map { |info, _| info[0] }.max

    while file_id >= 1
      file_index = disk_layout.each_with_index.filter { |info, _| info[0] == file_id }.map { |_, index| index }[0]
      file_length = disk_layout.filter { |id, _| id == file_id }.map { |_, l| l }[0]
      indexes_to_replace = disk_layout.each_with_index.filter { |info, index| info[0] == '.' && info[1] >= file_length && index < file_index }

      unless indexes_to_replace.empty?
        index_to_replace = indexes_to_replace[0][1]

        disk_layout = [
          *disk_layout.slice(0, index_to_replace),
          [file_id, file_length],
          ['.', indexes_to_replace[0][0][1] - file_length],
          *disk_layout.slice(index_to_replace + 1, file_index - index_to_replace - 1),
          ['.', file_length],
          *disk_layout.slice(file_index + 1, disk_layout.length - file_index)
        ].reduce([[], '.', 0]) { |acc, cur|
          if cur[0] == acc[1]
            [acc[0], acc[1], acc[2] + cur[1]]
          else
            [[*acc[0], [acc[1], acc[2]]], cur[0], cur[1]]
          end
        }

        disk_layout = [*disk_layout[0], [disk_layout[1], disk_layout[2]]].reject { |_, l| l == 0 }
      end

      file_id -= 1
    end

    get_disk_from_layout(disk_layout)
      .each_with_index
      .reject { |char, _| char == '.' }
      .reduce(0) { |acc, info| acc + info[0] * info[1] }
  end

  private

  def get_disk(input) = get_disk_from_layout(get_disk_layout(input))
  def get_disk_from_layout(disk_layout) = disk_layout.reduce([]) { |acc, vk| [*acc, *[vk[0]] * vk[1]] }

  def get_disk_layout(input) = input
    .chomp
    .split('')
    .map(&:to_i)
    .each_with_index
    .reduce([]) { |acc, vk|
      vk[1] % 2 == 0 ? [*acc, [vk[1] / 2, vk[0]]] : [*acc, ['.', vk[0]]]
    }.reject { |_, l| l == 0 }
end
