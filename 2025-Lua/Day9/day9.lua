package.path = package.path .. ";../?.lua"
require "Util/table"

local points = {}

for line in io.lines("input.txt") do
    local _, _, x, y = string.find(line, "(%d+),(%d+)")

    points[#points + 1] = {['x'] = tonumber(x), ['y'] = tonumber(y)}
end

local max_area = 0

for _, i in ipairs(points) do
    for _, j in ipairs(points) do
        local dx = math.abs(i['x'] - j['x']) + 1
        local dy = math.abs(i['y'] - j['y']) + 1

        if dx * dy > max_area then
            max_area = dx * dy
        end
    end
end

print("Part 1: " .. max_area)