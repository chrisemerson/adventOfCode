package.path = package.path .. ";../?.lua"
require "Util/table"

local points = {}
local left_bound = nil
local right_bound = 0
local top_bound = nil
local bottom_bound = 0

for line in io.lines("test.txt") do
    local _, _, x, y = string.find(line, "(%d+),(%d+)")

    points[#points + 1] = {['x'] = tonumber(x), ['y'] = tonumber(y)}
end

local max_area = 0

for _, i in ipairs(points) do
    for _, j in ipairs(points) do
        if left_bound == nil then
            left_bound = i['x']
        end

        if top_bound == nil then
            top_bound = i['y']
        end

        left_bound = math.min(left_bound, i['x'], j['x'])
        right_bound = math.max(right_bound, i['x'], j['x'])
        top_bound = math.min(top_bound, i['y'], j['y'])
        bottom_bound = math.max(bottom_bound, i['y'], j['y'])

        local dx = math.abs(i['x'] - j['x']) + 1
        local dy = math.abs(i['y'] - j['y']) + 1

        if dx * dy > max_area then
            max_area = dx * dy
        end
    end
end

print("Part 1: " .. max_area)

left_bound = left_bound
right_bound = right_bound
top_bound = top_bound
bottom_bound = bottom_bound

local grid = {}

for y = top_bound - 2, bottom_bound + 1 do
    for x = left_bound - 2, right_bound + 1 do
        if grid[y] == nil then
            grid[y] = {}
        end

        grid[y][x] = "."
    end
end

local last_point = points[#points]

for _, point in ipairs(points) do
    grid[point['y']][point['x']] = 'r'

    local step_y = (last_point['y'] - point['y']) / math.abs(last_point['y'] - point['y'])
    local step_x = (last_point['x'] - point['x']) / math.abs(last_point['x'] - point['x'])

    for dy = point['y'], last_point['y'], step_y do
        for dx = point['x'], last_point['x'], step_x do
            grid[dy][dx] = 'g'
        end
    end

    last_point = point
end

function print_grid(grid)
    for _, y in ipairs(grid) do
        for _, x in ipairs(y) do
            io.write(x)
        end

        print()
    end
end

function point_in_polygon(points, y, x)
    
end

--print_grid(grid)
