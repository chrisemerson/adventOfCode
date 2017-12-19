grid = []

current_dir = 'D'
letters_collected = ''
steps = 0

with open('input.txt', 'r') as fp:
    for line in fp:
        stripped = line.rstrip('\n')

        if len(stripped) > 0:
            grid.append(stripped)

position = [grid[0].index('|'), 0]


def make_move(position, direction):
    return {
        'U': [position[0], position[1] - 1],
        'D': [position[0], position[1] + 1],
        'L': [position[0] - 1, position[1]],
        'R': [position[0] + 1, position[1]]
    }[direction]


while True:
    if grid[position[1]][position[0]] == '+':
        if current_dir == 'U' or current_dir == 'D':
            if (position[0] + 1 == len(grid[position[1]]))\
                    or (grid[position[1]][position[0] + 1] == ' '):
                current_dir = 'L'
            else:
                current_dir = 'R'
        else:
            if (position[1] + 1 == len(grid))\
                    or (position[0] >= len(grid[position[1] + 1]))\
                    or (grid[position[1] + 1][position[0]] == ' '):
                current_dir = 'U'
            else:
                current_dir = 'D'
    elif grid[position[1]][position[0]] == ' ':
        break
    elif grid[position[1]][position[0]] != '|' and grid[position[1]][position[0]] != '-':
        letters_collected += grid[position[1]][position[0]]

    position = make_move(position, current_dir)
    steps += 1

print(letters_collected)
print(str(steps) + " steps taken")