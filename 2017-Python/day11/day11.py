directions = []

transforms = [
    {
        'from': ['se', 'nw'],
        'to': ''
    },
    {
        'from': ['sw', 'ne'],
        'to': ''
    },
    {
        'from': ['s', 'n'],
        'to': ''
    },
    {
        'from': ['ne', 's'],
        'to': 'se'
    },
    {
        'from': ['se', 'n'],
        'to': 'ne'
    },
    {
        'from': ['sw', 'n'],
        'to': 'nw'
    },
    {
        'from': ['nw', 's'],
        'to': 'sw'
    }
]

with open('input.txt', 'r') as fp:
    directions = list(fp.readline().strip().split(','))


def get_distance(directions, transforms):
    working_directions = directions[:]
    transform_occurred = True

    while transform_occurred:
        transform_occurred = False

        for transform in transforms:
            if transform['from'][0] in working_directions and transform['from'][1] in working_directions:
                del working_directions[working_directions.index(transform['from'][0])]
                del working_directions[working_directions.index(transform['from'][1])]

                if transform['to'] != '':
                    working_directions.append(transform['to'])

                transform_occurred = True

    return len(working_directions)


print("Part 1: " + str(get_distance(directions, transforms)))

max_dist = 0

for i in range(1, len(directions) - 1):
    max_dist = max(max_dist, get_distance(directions[:i], transforms))

print("Part 2: " + str(max_dist))