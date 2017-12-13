layers = {}

with open('input.txt', 'r') as fp:
    for line in fp:
        parts = line.split(':')
        layers[int(parts[0])] = {
            'range': int(parts[1].strip()),
            'current_scanner_pos': 0,
            'current_scanner_dir': 'D'
        }

delay = 0

while True:
    severity = 0
    position = 0
    caught = False

    for scanner in layers:
        scanner_pos = delay % ((layers[scanner]['range'] * 2) - 2)

        if scanner_pos >= (layers[scanner]['range'] - 1):
            layers[scanner]['current_scanner_pos'] = ((layers[scanner]['range'] * 2) - 2) - scanner_pos
            layers[scanner]['current_scanner_dir'] = 'U'
        else:
            layers[scanner]['current_scanner_pos'] = scanner_pos
            layers[scanner]['current_scanner_dir'] = 'D'

    while position <= max(layers.keys()):
        if position in layers.keys() and layers[position]['current_scanner_pos'] == 0:
            severity += position * layers[position]['range']
            caught = True

            if delay > 0:
                break

        for scanner in layers:
            if layers[scanner]['current_scanner_dir'] == 'D':
                layers[scanner]['current_scanner_pos'] += 1
            else:
                layers[scanner]['current_scanner_pos'] -= 1

            if layers[scanner]['current_scanner_pos'] == 0:
                layers[scanner]['current_scanner_dir'] = 'D'
            elif layers[scanner]['current_scanner_pos'] == (layers[scanner]['range'] - 1):
                layers[scanner]['current_scanner_dir'] = 'U'

        position += 1

    if delay == 0:
        print("Part 1: " + str(severity))

    if not caught:
        break

    delay += 1

print("Part 2: " + str(delay))
