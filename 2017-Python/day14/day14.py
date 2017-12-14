import os
import sys

sys.path.append(os.path.abspath('..'))
sys.setrecursionlimit(128 * 128)

from util.knothash import knot_hash

string = sys.argv[1]
grid = []

for i in range(0, 128):
    grid.append(knot_hash.knot_hash(string + '-' + str(i)))

grid = list(map(lambda x: bin(int(x, 16))[2:].zfill(128), grid))

used_squares = 0

for line in grid:
    used_squares += len(list(filter(lambda x: x == '1', list(line))))

print("Used squares: " + str(used_squares))

squares_assigned_region = []
regions = {}


def define_region(x, y, regionid):
    if grid[y][x] == '0':
        return False

    if str(x) + '-' + str(y) in squares_assigned_region:
        return False

    if regionid not in regions.keys():
        regions[regionid] = []

    regions[regionid].append([x, y])
    squares_assigned_region.append(str(x) + '-' + str(y))

    if 0 <= (x - 1) <= 127:
        define_region(x - 1, y, regionid)
    if 0 <= (y - 1) <= 127:
        define_region(x, (y - 1), regionid)
    if 0 <= (y + 1) <= 127:
        define_region(x, (y + 1), regionid)
    if 0 <= (x + 1) <= 127:
        define_region(x + 1, y, regionid)


region_id = 1

for row in range(0, len(grid)):
    for col in range(0, len(grid[row])):
        if grid[row][col] == '1' and [col,row] not in squares_assigned_region:
            define_region(col, row, region_id)

        region_id += 1

print(len(regions))