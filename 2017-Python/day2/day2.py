spreadsheet = open('input.txt', 'r')

total = 0

with spreadsheet as fp:
    for line in fp:
        numbers = list(map(int, line.strip().split("\t")))
        total += max(numbers) - min(numbers)

print(total)