b = 108100
c = 125100
f = True
h = 0

# Do this 1001 times
while True:
    f = True

    for i in range(2, b):
        if b % i == 0:
            f = False

    if not f:
        h += 1

    if b == c:
        break

    b += 17

print(h)
