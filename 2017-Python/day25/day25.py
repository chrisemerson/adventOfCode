import re

current_state = None
steps = 0
state_machine = {}
cursor = 0
tape = {cursor: 0}

currently_interpreting_state = None
currently_interpreting_value = None

with open('input.txt', 'r') as fp:
    for line in fp:
        matches = re.search('^Begin in state ([A-Z])\.$', line)
        if matches:
            current_state = matches.group(1)

        matches = re.search('^Perform a diagnostic checksum after (\d+) steps.$', line)
        if matches:
            steps = int(matches.group(1))

        matches = re.search('^In state ([A-Z]):$', line)
        if matches:
            currently_interpreting_state = matches.group(1)
            state_machine[currently_interpreting_state] = {}

        matches = re.search('^\s+If the current value is (\d):$', line)
        if matches:
            currently_interpreting_value = int(matches.group(1))
            state_machine[currently_interpreting_state][currently_interpreting_value] = {
                'write': None,
                'move': '',
                'new_state': None
            }

        matches = re.search('^\s+- Write the value (\d).$', line)
        if matches:
            state_machine[currently_interpreting_state][currently_interpreting_value]['write'] = int(matches.group(1))

        matches = re.search('^\s+- Move one slot to the (left|right).$', line)
        if matches:
            if matches.group(1) == 'left':
                state_machine[currently_interpreting_state][currently_interpreting_value]['move'] = 'L'
            else:
                state_machine[currently_interpreting_state][currently_interpreting_value]['move'] = 'R'

        matches = re.search('^\s+- Continue with state ([A-Z]).$', line)
        if matches:
            state_machine[currently_interpreting_state][currently_interpreting_value]['new_state'] = matches.group(1)

for i in range(0, steps):
    current_val = tape[cursor]

    tape[cursor] = state_machine[current_state][current_val]['write']

    if state_machine[current_state][current_val]['move'] == 'L':
        cursor -= 1
    else:
        cursor += 1

    if cursor not in tape:
        tape[cursor] = 0

    current_state = state_machine[current_state][current_val]['new_state']

checksum = 0

for pos in tape:
    if tape[pos] == 1:
        checksum += 1

print(checksum)