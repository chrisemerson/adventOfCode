programs = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p']

with open('input.txt', 'r') as fp:
    line = fp.readline()
    instructions = list(line.strip().split(','))

# Testing showed that programs repeat their ordering after 30 iterations of the dance
for i in range(0,1000000000 % 30):
    for instruction in instructions:
        if instruction[0] == 's':
            places = int(instruction[1:])
            programs = programs[0 - places:] + programs[:0 - places]

        elif instruction[0] == 'x':
            pieces = instruction[1:].split('/')
            index1 = int(pieces[0])
            index2 = int(pieces[1])

            item1 = programs[index1]
            item2 = programs[index2]

            programs[index1] = item2
            programs[index2] = item1

        elif instruction[0] == 'p':
            pieces = instruction[1:].split('/')
            program1 = pieces[0]
            program2 = pieces[1]

            index1 = programs.index(program1)
            index2 = programs.index(program2)

            programs[index1] = program2
            programs[index2] = program1

print(''.join(programs))