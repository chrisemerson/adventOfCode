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


def get_val(registers, identifier):
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
        sounds_played.append(get_val(registers, register))
    elif command == 'set':
        registers[register] = get_val(registers, matches.group(3))
    elif command == 'add':
        registers[register] += get_val(registers, matches.group(3))
    elif command == 'mul':
        registers[register] *= get_val(registers, matches.group(3))
    elif command == 'mod':
        registers[register] %= get_val(registers, matches.group(3))
    elif command == 'rcv':
        non_zero_sounds_played = list(filter(lambda x: x != 0, sounds_played))
        last_freq_recovered = non_zero_sounds_played[len(non_zero_sounds_played) - 1]
        break
    elif command == 'jgz':
        if get_val(registers, register) > 0:
            current_position += (get_val(registers, matches.group(3)) - 1)

    current_position += 1

print(last_freq_recovered)

registers_0 = {'p': 0}
registers_1 = {'p': 1}

position_0 = 0
position_1 = 0

blocked_0 = False
blocked_1 = False

message_queue_0 = []
message_queue_1 = []

program_1_values_sent = 0

while (0 <= position_0 < len(instructions)) and (0 <= position_1 < len(instructions)) and (not blocked_0) and (not blocked_1):
    instruction_0 = instructions[position_0]
    instruction_1 = instructions[position_1]

    matches_0 = re.search('^(snd|set|add|mul|mod|rcv|jgz)\s+([A-Za-z0-9]+)(?:\s+(-?[A-Za-z0-9]+))?$', instruction_0)
    matches_1 = re.search('^(snd|set|add|mul|mod|rcv|jgz)\s+([A-Za-z0-9]+)(?:\s+(-?[A-Za-z0-9]+))?$', instruction_1)

    command_0 = matches_0.group(1)
    register_0 = matches_0.group(2)
    command_1 = matches_1.group(1)
    register_1 = matches_1.group(2)

    if register_0 not in registers_0:
        registers_0[register_0] = 0

    if register_1 not in registers_1:
        registers_1[register_1] = 0

    if command_0 == 'snd':
        message_queue_1.append(get_val(registers_0, register_0))
    elif command_0 == 'set':
        registers_0[register_0] = get_val(registers_0, matches_0.group(3))
    elif command_0 == 'add':
        registers_0[register_0] += get_val(registers_0, matches_0.group(3))
    elif command_0 == 'mul':
        registers_0[register_0] *= get_val(registers_0, matches_0.group(3))
    elif command_0 == 'mod':
        registers_0[register_0] %= get_val(registers_0, matches_0.group(3))
    elif command_0 == 'rcv':
        blocked_0 = True

        if len(message_queue_0) > 0:
            registers_0[register_0] = message_queue_0.pop(0)
            blocked_0 = False

    elif command_0 == 'jgz':
        if get_val(registers_0, register_0) > 0:
            position_0 += (get_val(registers_0, matches_0.group(3)) - 1)

    if not blocked_0:
        position_0 += 1

    if command_1 == 'snd':
        message_queue_0.append(get_val(registers_1, register_1))
        program_1_values_sent += 1
    elif command_1 == 'set':
        registers_1[register_1] = get_val(registers_1, matches_1.group(3))
    elif command_1 == 'add':
        registers_1[register_1] += get_val(registers_1, matches_1.group(3))
    elif command_1 == 'mul':
        registers_1[register_1] *= get_val(registers_1, matches_1.group(3))
    elif command_1 == 'mod':
        registers_1[register_1] %= get_val(registers_1, matches_1.group(3))
    elif command_1 == 'rcv':
        blocked_1 = True

        if len(message_queue_1) > 0:
            registers_1[register_1] = message_queue_1.pop(0)
            blocked_1 = False

    elif command_1 == 'jgz':
        if get_val(registers_1, register_1) > 0:
            position_1 += (get_val(registers_1, matches_1.group(3)) - 1)

    if not blocked_1:
        position_1 += 1

print("Program 1 sent a value " + str(program_1_values_sent) + " times")