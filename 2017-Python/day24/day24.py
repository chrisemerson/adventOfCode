components = []

with open('input.txt', 'r') as fp:
    for line in fp:
        ports = line.strip().split('/')
        components.append([int(ports[0]), int(ports[1])])


def get_strongest_bridge(components, open_port):
    if len(components) == 0:
        return 0

    max_score = 0

    for i in range(0, len(components)):
        if components[i][0] == open_port or components[i][1] == open_port:
            score = components[i][0] + components[i][1]

            if components[i][0] == open_port:
                score += get_strongest_bridge(components[:i] + components[i + 1:], components[i][1])
            elif components[i][1] == open_port:
                score += get_strongest_bridge(components[:i] + components[i + 1:], components[i][0])

            if score > max_score:
                max_score = score

    return max_score


print(get_strongest_bridge(components, 0))


def get_longest_bridge(components, open_port):
    if len(components) == 0:
        return (0, 0)

    max_length = 0
    max_score = 0

    for i in range(0, len(components)):
        if components[i][0] == open_port or components[i][1] == open_port:
            score = components[i][0] + components[i][1]
            length = 1

            if components[i][0] == open_port:
                result = get_longest_bridge(components[:i] + components[i + 1:], components[i][1])
            elif components[i][1] == open_port:
                result = get_longest_bridge(components[:i] + components[i + 1:], components[i][0])

            length += result[0]
            score += result[1]

            if length > max_length:
                max_length = length
                max_score = score
            elif length == max_length and score > max_score:
                max_score = score

    return (max_length, max_score)


print(get_longest_bridge(components, 0))
