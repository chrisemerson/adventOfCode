import re
import math
import time

start = time.time()
instructions = {}
starting_state = '.#./..#/###'


def get_size(state):
    return int(math.sqrt(len(state.replace('/', ''))))


def get_sub_grid_key(line, char, split_size, sub_grids_per_row):
    return int(math.floor(line / split_size) * sub_grids_per_row + math.floor(char / split_size))


def split_grid(state, split_size):
    sub_grids = {}
    line_no = 0

    for line in state.split('/'):
        char_no = 0
        sub_grids_appended_to = set()

        for char in line:
            sub_grid_id = get_sub_grid_key(line_no, char_no, split_size, get_size(state) / split_size)
            sub_grids_appended_to.add(sub_grid_id)

            if sub_grid_id not in sub_grids:
                sub_grids[sub_grid_id] = ''

            sub_grids[sub_grid_id] += char
            char_no += 1

        for sub_grid_id in sub_grids_appended_to:
            sub_grids[sub_grid_id] += '/'

        line_no += 1

    for sub_grid_id in sub_grids:
        sub_grids[sub_grid_id] = strip_trailing_slash(sub_grids[sub_grid_id])

    return sub_grids


def rotate_grid(grid):
    grid_lines = grid.split('/')

    if len(grid_lines[0]) == 2:
        return grid_lines[1][0] + grid_lines[0][0] + '/' + grid_lines[1][1] + grid_lines[0][1]
    else:
        return\
            grid_lines[2][0] +\
            grid_lines[1][0] +\
            grid_lines[0][0] +\
            '/' +\
            grid_lines[2][1] +\
            grid_lines[1][1] +\
            grid_lines[0][1] +\
            '/' +\
            grid_lines[2][2] +\
            grid_lines[1][2] +\
            grid_lines[0][2]


def reflect_grid(grid):
    grid_lines = grid.split('/')
    reflected_grid = ''

    for i in range(0, len(grid_lines)):
        reflected_grid += grid_lines[len(grid_lines) - i - 1] + '/'

    return strip_trailing_slash(reflected_grid)


def get_reflections_and_rotations(grid):
    reflections_and_rotations = []
    reflected_grid = reflect_grid(grid)

    reflections_and_rotations.append(grid)
    reflections_and_rotations.append(rotate_grid(grid))
    reflections_and_rotations.append(rotate_grid(rotate_grid(grid)))
    reflections_and_rotations.append(rotate_grid(rotate_grid(rotate_grid(grid))))

    reflections_and_rotations.append(reflected_grid)
    reflections_and_rotations.append(rotate_grid(reflected_grid))
    reflections_and_rotations.append(rotate_grid(rotate_grid(reflected_grid)))
    reflections_and_rotations.append(rotate_grid(rotate_grid(rotate_grid(reflected_grid))))

    return reflections_and_rotations


def transform_state(grid):
    return instructions[grid]


def strip_trailing_slash(string):
    return re.sub('^(.*)/$', '\\1', string)


def reassemble_grid(sub_grids):
    if len(sub_grids) == 1:
        return sub_grids[0]

    grid_size = int(get_size(sub_grids[0]) * math.sqrt(len(sub_grids)))
    split_size = get_size(sub_grids[0])
    grid = ''

    for line in range(0, grid_size):
        for char in range(0, grid_size):
            sub_grid_id = get_sub_grid_key(line, char, split_size, int(math.sqrt(len(sub_grids))))
            sub_grid = sub_grids[sub_grid_id].split('/')
            grid += sub_grid[line % split_size][char % split_size]

        grid += '/'

    return strip_trailing_slash(grid)


def get_next_state(state):
    if get_size(state) % 2 == 0:
        split_size = 2
    else:
        split_size = 3

    sub_grids = split_grid(state, split_size)

    for sub_grid_id in sub_grids:
        sub_grids[sub_grid_id] = transform_state(sub_grids[sub_grid_id])

    state = reassemble_grid(sub_grids)

    return state


def count_pixels(state):
    grid_lines = state.split('/')
    total_on_pixels = 0

    for line in grid_lines:
        total_on_pixels += len(line.replace('.', ''))

    return total_on_pixels


with open('input.txt', 'r') as fp:
    for line in fp:
        matches = re.search('^([.#/]*)\s+=>\s+([.#/]*)$', line)

        for reflection_or_rotation in get_reflections_and_rotations(matches.group(1)):
            instructions[reflection_or_rotation] = matches.group(2)

pt_1 = starting_state

for i in range(0, 5):
    pt_1 = get_next_state(pt_1)
    print(pt_1)

print(count_pixels(pt_1))

pt_2 = starting_state

for i in range(0, 18):
    print("Starting iteration " + str(i + 1) + " (" + str(time.time() - start) + ")")
    pt_2 = get_next_state(pt_2)

print(count_pixels(pt_2))
