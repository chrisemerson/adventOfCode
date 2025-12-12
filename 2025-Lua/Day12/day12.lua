package.path = package.path .. ";../?.lua;../?/init.lua"
require "Util/table"

local current_shape_label = 0
local shapes = {}
local shape_data = {}
local problems = {}

for line in io.lines("input.txt") do
    if string.match(line, "^(%d+):$") then
        current_shape_label = string.match(line, "^(%d+):$")
    elseif string.match(line, "^([.#]+)$") then
        shape_data[#shape_data + 1] = string.match(line, "^([.#]+)$")
    elseif string.match(line, "^$") then
        shapes[current_shape_label] = copy(shape_data)
        shape_data = {}
    elseif string.match(line, "^(%d+)x(%d+):([%d%s]+)$") then
        local row, col, gift_counts = string.match(line, "^(%d+)x(%d+):([%d%s]+)$")

        local gift_requirements = {}

        for count in string.gmatch(gift_counts, "(%d+)") do
            gift_requirements[#gift_requirements + 1] = count
        end

        problems[#problems + 1] = { ['row'] = row, ['col'] = col, ['gift_counts'] = gift_requirements }
    end
end

local total_present_footprints = {}

for i, shape in pairs(shapes) do
    local this_shape_footprint = 0

    for _, line in ipairs(shape) do
        for i = 1, #line do
            if string.sub(line, i, i) == "#" then
                this_shape_footprint = this_shape_footprint + 1
            end
        end
    end

    total_present_footprints[i + 1] = this_shape_footprint
end

local possible_arrangements = 0
local impossible_arrangements = 0

for _, problem in ipairs(problems) do
    local total_footprint_requirements = 0
    local total_gift_count = 0

    for i, gift_requirement in pairs(problem['gift_counts']) do
        total_footprint_requirements = total_footprint_requirements + (total_present_footprints[i] * gift_requirement)
        total_gift_count = total_gift_count + gift_requirement
    end

    if total_footprint_requirements > problem['row'] * problem['col'] then
        impossible_arrangements = impossible_arrangements + 1
    end

    if math.floor(problem['row'] / 3) * math.floor(problem['col'] / 3) >= total_gift_count then
        possible_arrangements = possible_arrangements + 1
    end
end

if #problems == impossible_arrangements + possible_arrangements then
    print("Part 1: " .. possible_arrangements)
else
    print("ERROR! Something has gone wrong")
end
