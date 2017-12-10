import string
from functools import reduce

number_list = list(range(0, 256))
number_list_pt2 = list(range(0, 256))
current_position = 0
current_position_pt2 = 0
skip_size = 0
skip_size_pt2 = 0
lengths = []
lengths_pt2 = []

with open('input.txt', 'r') as fp:
    line = fp.readline().strip()
    lengths = list(map(int, line.split(',')))
    lengths_pt2 = list(map(ord, list(line))) + [17, 31, 73, 47, 23]


def reverse_elements(number_list, current_position, length):
    if length <= 1:
        return number_list

    elif current_position + length <= len(number_list):
        sub_list = number_list[current_position:current_position + length]

        reversed_sublist = list(reversed(sub_list))

        return number_list[0:current_position] + reversed_sublist + number_list[current_position + length:]

    else:
        sub_list_start = number_list[current_position:]
        sub_list_end = number_list[0:length - len(sub_list_start)]

        sub_list = sub_list_start + sub_list_end

        reversed_sublist = list(reversed(sub_list))

        return\
            reversed_sublist[len(sub_list_start):]\
            + number_list[len(sub_list_end):(len(sub_list_end) + len(number_list) - length)]\
            + reversed_sublist[:len(sub_list_start)]


for length in lengths:
    number_list = reverse_elements(number_list, current_position, length)
    current_position = (current_position + length + skip_size) % len(number_list)
    skip_size += 1

print("Answer Pt 1: " + str(number_list[0] * number_list[1]))

for i in range(0, 64):
    for length in lengths_pt2:
        number_list_pt2 = reverse_elements(number_list_pt2, current_position_pt2, length)
        current_position_pt2 = (current_position_pt2 + length + skip_size_pt2) % len(number_list_pt2)
        skip_size_pt2 += 1

dense_hash = []

for i in range(0, 16):
    sub_list = number_list_pt2[16 * i:16 * (i + 1)]
    dense_hash.append(reduce(lambda a, b: a ^ b, sub_list))

hex_hash = list(map(lambda x: format(x, 'x'), dense_hash))
print("Answer Pt 2: " + ''.join(hex_hash))