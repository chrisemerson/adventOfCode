import re

instructions = []
registers = {}
current_position = 0
multiplications = 0

with open('input.txt', 'r') as fp:
    for line in fp:
        instructions.append(line.strip())


def is_int(string):
    try:
        int(string)
        return True
    except ValueError:
        return False


def get_val(registers, identifier):
    if is_int(identifier):
        return int(identifier)
    else:
        return registers[identifier]


while 0 <= current_position < len(instructions):
    instruction = instructions[current_position]
    matches = re.search('^(set|sub|mul|jnz)\s+([A-Za-z0-9]+)(?:\s+(-?[A-Za-z0-9]+))?$', instruction)
    command = matches.group(1)
    register = matches.group(2)

    if register not in registers:
        registers[register] = 0

    if command == 'set':
        registers[register] = get_val(registers, matches.group(3))
    elif command == 'sub':
        registers[register] -= get_val(registers, matches.group(3))
    elif command == 'mul':
        registers[register] *= get_val(registers, matches.group(3))
        multiplications += 1
    elif command == 'jnz':
        if get_val(registers, register) != 0:
            current_position += (get_val(registers, matches.group(3)) - 1)

    current_position += 1

print(multiplications)