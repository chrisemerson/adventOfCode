grid = {}
grid_pt2 = {}

with open('input.txt', 'r') as fp:
    line_no = 0

    for line in fp:
        char_no = 0

        for char in line:
            grid[(char_no, line_no)] = char
            grid_pt2[(char_no, line_no)] = char
            char_no += 1

        line_no += 1

position = ((line_no - 1) / 2, (line_no - 1) / 2)
direction = 'N'
steps = 10000
steps_pt2 = 10000000
infections = 0
infections_pt2 = 0


def turn_right(current_dir):
    return {
        'N': 'E',
        'E': 'S',
        'S': 'W',
        'W': 'N'
    }[current_dir]


def turn_left(current_dir):
    return {
        'N': 'W',
        'E': 'N',
        'S': 'E',
        'W': 'S'
    }[current_dir]


def turn_round(current_dir):
    return {
        'N': 'S',
        'E': 'W',
        'S': 'N',
        'W': 'E'
    }[current_dir]


def move(position, direction):
    return {
        'N': (position[0], position[1] - 1),
        'E': (position[0] + 1, position[1]),
        'S': (position[0], position[1] + 1),
        'W': (position[0] - 1, position[1])
    }[direction]


for i in range(0, steps):
    if grid[position] == '#':
        direction = turn_right(direction)
        grid[position] = '.'
    else:
        direction = turn_left(direction)
        grid[position] = '#'
        infections += 1

    position = move(position, direction)

    if position not in grid:
        grid[position] = '.'

print(infections)

position = ((line_no - 1) / 2, (line_no - 1) / 2)
direction = 'N'

for i in range(0, steps_pt2):
    if grid_pt2[position] == '#':
        direction = turn_right(direction)
        grid_pt2[position] = 'F'

    elif grid_pt2[position] == 'W':
        grid_pt2[position] = '#'
        infections_pt2 += 1

    elif grid_pt2[position] == 'F':
        direction = turn_round(direction)
        grid_pt2[position] = '.'

    else:
        direction = turn_left(direction)
        grid_pt2[position] = 'W'

    position = move(position, direction)

    if position not in grid_pt2:
        grid_pt2[position] = '.'

print(infections_pt2)
