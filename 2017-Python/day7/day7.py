import re
from collections import Counter

programs = {}


def find_root(program_name, programs):
    for program in programs:
        if program_name in programs[program]['children']:
            return find_root(program, programs)

    return program_name


def get_weight(program_name, programs):
    total_weight = programs[program_name]['weight']

    if programs[program_name]['children']:
        for child in programs[program_name]['children']:
            total_weight += get_weight(child, programs)

    return total_weight


def get_child_weights(program_name, programs):
    child_weights = {}

    if programs[program_name]['children']:
        for child in programs[program_name]['children']:
            child_weights[child] = get_weight(child, programs)

    return child_weights


def find_unbalanced_element(program_name, programs):
    child_weights = get_child_weights(program_name, programs)
    count = Counter(child_weights.values())

    bad_weight = 0

    for index in count:
        if count[index] == 1:
            bad_weight = index

    for program in child_weights:
        if child_weights[program] == bad_weight:
            return program

    return None


with open('input.txt', 'r') as fp:
    for line in fp:
        matches = re.search('^(.*)\s+\((\d+)\)(?:\s+->\s+(.*))?$', line)

        children = {}

        if matches.group(3):
            childList = list(map(lambda x: x.strip(), matches.group(3).split(',')))

            for child in childList:
                children[child] = {}

        programs[matches.group(1)] = {'weight': int(matches.group(2)), 'children': children}

root_element = find_root(next(iter(programs.keys())), programs)

print("Root element: " + root_element)

unbalanced_element = find_unbalanced_element(root_element, programs)
unbalanced_element_parent = root_element

while find_unbalanced_element(unbalanced_element, programs) is not None:
    unbalanced_element_parent = unbalanced_element
    unbalanced_element = find_unbalanced_element(unbalanced_element, programs)

unbalanced_children_weights = get_child_weights(unbalanced_element_parent, programs)
current_unbalanced_weight = unbalanced_children_weights[unbalanced_element]

balanced_child_weight = 0

for child in unbalanced_children_weights:
    if unbalanced_children_weights[child] != current_unbalanced_weight:
        balanced_child_weight = unbalanced_children_weights[child]
        break

difference_in_weight_needed = balanced_child_weight - current_unbalanced_weight

print(
    "Correct weight for element "
    + unbalanced_element
    + " should be "
    + str(programs[unbalanced_element]['weight'] + difference_in_weight_needed)
)