import re

programs = {}
visited = []
set = set()
groups = []

with open('input.txt', 'r') as fp:
    for line in fp:
        matches = re.search('^(\d+)\s+\<\-\>\s+(.*)$', line.strip())
        programs[int(matches.group(1))] = list(map(int, matches.group(2).split(",")))


def add_to_set(program, programs, set, visited):
    set.add(program)
    visited.append(program)

    for sub_program in programs[program]:
        if sub_program not in visited:
            add_to_set(sub_program, programs, set, visited)

    return set


print(len(add_to_set(0, programs, set, visited)))

groups.append(add_to_set(0, programs, set, visited))

for i in range(0, len(programs)):
    already_in_set = False

    for group in groups:
        if i in group:
            already_in_set = True

    if not already_in_set:
        groups.append(add_to_set(i, programs, set, visited))

print(len(groups))