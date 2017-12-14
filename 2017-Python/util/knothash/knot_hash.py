from functools import reduce
from util.knothash import reverse_elements


def knot_hash(string):
    number_list = list(range(0, 256))
    skip_size = 0
    current_position = 0
    lengths = list(map(ord, list(string))) + [17, 31, 73, 47, 23]

    for i in range(0, 64):
        for length in lengths:
            number_list = reverse_elements.reverse_elements(number_list, current_position, length)
            current_position = (current_position + length + skip_size) % len(number_list)
            skip_size += 1

    dense_hash = []

    for i in range(0, 16):
        sub_list = number_list[16 * i:16 * (i + 1)]
        dense_hash.append(reduce(lambda a, b: a ^ b, sub_list))

    hex_hash = list(map(lambda x: format(x, 'x'), dense_hash))

    return ''.join(hex_hash)