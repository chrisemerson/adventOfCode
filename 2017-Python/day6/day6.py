states = []
index_of_repeated_state = 0


def progress_state(numbers: list):
    next_state = numbers[:]

    number_to_redistribute = max(next_state)
    index_of_number_to_redistribute = next_state.index(number_to_redistribute)

    next_state[index_of_number_to_redistribute] = 0
    current_index = index_of_number_to_redistribute + 1

    while number_to_redistribute > 0:
        next_state[current_index % len(numbers)] += 1

        current_index += 1
        number_to_redistribute -= 1

    return next_state


with open('input.txt', 'r') as fp:
    line = fp.readline()
    numbers = list(map(int, line.strip().split("\t")))

states.append(numbers)
keep_going = True

while keep_going:
    next_state = progress_state(states[len(states) - 1])

    for i in range(len(states)):
        state = states[i]

        if state == next_state:
            index_of_repeated_state = i
            keep_going = False

    states.append(next_state)

print(len(states) - 1)
print(len(states) - 1 - index_of_repeated_state)