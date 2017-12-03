import sys
import math

number = int(sys.argv[1])

if number > 1:
    layer = math.ceil((math.sqrt(number) - 1) / 2)
    distFromKey = number - ((2 * layer - 1) ** 2 + layer)
    position = min(distFromKey % (2 * layer), 2 * layer - (distFromKey % (2 * layer)))
    distance = layer + position
else:
    distance = 0

print("Distance: " + str(distance))
