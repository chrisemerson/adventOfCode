import sys

gen_a_start = int(sys.argv[1])
gen_b_start = int(sys.argv[2])

gen_a_val = gen_a_start
gen_b_val = gen_b_start

matches = 0

for i in range(0, 40000000):
    gen_a_val = (gen_a_val * 16807) % 2147483647
    gen_b_val = (gen_b_val * 48271) % 2147483647

    if gen_a_val % 65536 == gen_b_val % 65536:
        matches += 1

print("Part 1 matches: " + str(matches))


def gen_a_pt2(start, number):
    val = start
    count = 0

    while count < number:
        val = (val * 16807) % 2147483647

        if val % 4 == 0:
            yield val
            count += 1


def gen_b_pt2(start, number):
    val = start
    count = 0

    while count < number:
        val = (val * 48271) % 2147483647

        if val % 8 == 0:
            yield val
            count += 1


matches = 0
gen_a_val_generator = gen_a_pt2(gen_a_start, 5000000)
gen_b_val_generator = gen_b_pt2(gen_b_start, 5000000)

for i in range(0, 5000000):
    if next(gen_a_val_generator) % 65536 == next(gen_b_val_generator) % 65536:
        matches += 1

print("Part 2 matches: " + str(matches))