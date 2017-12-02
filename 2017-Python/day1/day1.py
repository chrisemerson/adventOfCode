string = open('input.txt', 'r').read()

total = 0
totalpt2 = 0

for i in range(0, len(string)):
    if string[i % len(string)] == string[(i + 1) % len(string)]:
        total += int(string[i % len(string)])

    if string[i % len(string)] == string[(i + int(len(string) / 2)) % len(string)]:
        totalpt2 += int(string[i % len(string)])

print(total)
print(totalpt2)