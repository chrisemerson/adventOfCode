import re

programs = {}


def find_root(program_name, programs):
    for program in programs:
        if program_name in programs[program]['children']:
            return find_root(program, programs)

    return program_name


with open('input.txt', 'r') as fp:
    for line in fp:
        matches = re.search('^(.*)\s+\((\d+)\)(?:\s+->\s+(.*))?$', line)

        children = {}

        if matches.group(3):
            childList = list(map(lambda x: x.strip(), matches.group(3).split(',')))

            for child in childList:
                children[child] = {}

        programs[matches.group(1)] = {'weight': (matches.group(2)), 'children': children}

root_element = find_root(next(iter(programs.keys())), programs)

print(root_element)