import os
import sys

sys.path.append(os.path.abspath('..'))

from util.knothash import knot_hash, reverse_elements

number_list = list(range(0, 256))
current_position = 0
skip_size = 0
lengths = []

with open('input.txt', 'r') as fp:
    line = fp.readline().strip()
    lengths = list(map(int, line.split(',')))

for length in lengths:
    number_list = reverse_elements.reverse_elements(number_list, current_position, length)
    current_position = (current_position + length + skip_size) % len(number_list)
    skip_size += 1

print("Answer Pt 1: " + str(number_list[0] * number_list[1]))
print("Answer Pt 2: " + knot_hash.knot_hash(line))