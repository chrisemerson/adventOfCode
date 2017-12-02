spreadsheet = open('input.txt', 'r')

total = 0
totalpt2 = 0

with spreadsheet as fp:
    for line in fp:
        numbers = list(map(int, line.strip().split("\t")))
        total += max(numbers) - min(numbers)

        for number in numbers:
            for innernumber in numbers:
                if number > innernumber and number % innernumber == 0:
                    totalpt2 += int(number / innernumber)

print(total)
print(totalpt2)