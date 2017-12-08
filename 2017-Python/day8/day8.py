import re

instructions = []
registers = {}
maxvalue = 0


def init_register(register, registers):
    if register not in registers:
        registers[register] = 0


def condition(register, comparator, value, registers):
    init_register(register, registers)

    if comparator == '>':
        return registers[register] > value
    elif comparator == '<':
        return registers[register] < value
    elif comparator == '>=':
        return registers[register] >= value
    elif comparator == '<=':
        return registers[register] <= value
    elif comparator == '==':
        return registers[register] == value
    elif comparator == '!=':
        return registers[register] != value


with open('input.txt', 'r') as fp:
    for line in fp:
        matches = re.search('^(.*?)\s+(inc|dec)\s+(-?\d+)\s+if\s+(.*?)\s+(.*?)\s+(-?\d+)$', line)

        instructions.append({
            'register': matches.group(1),
            'action': matches.group(2),
            'amount': int(matches.group(3)),
            'condition-register': matches.group(4),
            'condition-comparator': matches.group(5),
            'condition-value': int(matches.group(6))
        })

for instruction in instructions:
    init_register(instruction['register'], registers)

    if condition(
        instruction['condition-register'],
        instruction['condition-comparator'],
        instruction['condition-value'],
        registers
    ):
        if instruction['action'] == 'inc':
            registers[instruction['register']] += instruction['amount']
        elif instruction['action'] == 'dec':
            registers[instruction['register']] -= instruction['amount']

        maxvalue = max(maxvalue, registers[instruction['register']])

print(max(registers.values()))
print(maxvalue)