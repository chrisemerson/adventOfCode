currentpositionpt1 = 0
currentpositionpt2 = 0

stepspt1 = 0
stepspt2 = 0

instructionspt1 = []
instructionspt2 = []

with open('input.txt', 'r') as fp:
    for line in fp:
        instructionspt1.append(int(line))
        instructionspt2.append(int(line))

while 0 <= currentpositionpt1 < len(instructionspt1):
    jump = instructionspt1[currentpositionpt1]
    instructionspt1[currentpositionpt1] += 1
    currentpositionpt1 += jump
    stepspt1 += 1

while 0 <= currentpositionpt2 < len(instructionspt2):
    jump = instructionspt2[currentpositionpt2]

    if jump >= 3:
        instructionspt2[currentpositionpt2] -= 1
    else:
        instructionspt2[currentpositionpt2] += 1

    currentpositionpt2 += jump
    stepspt2 += 1

print("Part 1: " + str(stepspt1))
print("Part 2: " + str(stepspt2))