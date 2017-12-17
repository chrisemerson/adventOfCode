import sys

step = int(sys.argv[1])

buffer = [0]
current_position = 0

for i in range(1, 2018):
    current_position = ((current_position + step) % len(buffer)) + 1
    buffer = buffer[0:current_position] + [i] + buffer[current_position:]

print("Number after 2017: " + str(buffer[buffer.index(2017) + 1]))

current_position = 0
num_after_zero = 0

for i in range(1, 50000001):
    current_position = ((current_position + step) % i) + 1

    if current_position == 1:
        num_after_zero = i

print("Number after 0: " + str(num_after_zero))