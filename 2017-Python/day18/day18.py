import re

instructions = []
registers = {}
sounds_played = []
last_freq_recovered = 0
current_position = 0

with open('input.txt', 'r') as fp:
    for line in fp:
        instructions.append(line.strip())


def is_int(string):
    try:
        int(string)
        return True
    except ValueError:
        return False


def get_val(identifier):
    if is_int(identifier):
        return int(identifier)
    else:
        return registers[identifier]


while 0 <= current_position < len(instructions):
    instruction = instructions[current_position]
    matches = re.search('^(snd|set|add|mul|mod|rcv|jgz)\s+([A-Za-z0-9]+)(?:\s+(-?[A-Za-z0-9]+))?$', instruction)
    command = matches.group(1)
    register = matches.group(2)

    if register not in registers:
        registers[register] = 0

    if command == 'snd':
        sounds_played.append(get_val(register))
    elif command == 'set':
        registers[register] = get_val(matches.group(3))
    elif command == 'add':
        registers[register] += get_val(matches.group(3))
    elif command == 'mul':
        registers[register] *= get_val(matches.group(3))
    elif command == 'mod':
        registers[register] %= get_val(matches.group(3))
    elif command == 'rcv':
        non_zero_sounds_played = list(filter(lambda x: x != 0, sounds_played))
        last_freq_recovered = non_zero_sounds_played[len(non_zero_sounds_played) - 1]
        break
    elif command == 'jgz':
        if get_val(register) > 0:
            current_position += (get_val(matches.group(3)) - 1)

    current_position += 1

print(last_freq_recovered)