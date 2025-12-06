package.path = package.path .. ";../?.lua"

require "Util/table"

local ranges = {}
local ingredients = {}
local loading_ranges = true

for line in io.lines("input.txt") do
    if line == "" then
        loading_ranges = false
    elseif loading_ranges then
        _, _, min, max = string.find(line, "(%d+)%-(%d+)")
        ranges[#ranges + 1] = {['min'] = tonumber(min), ['max'] = tonumber(max)}
    else
        ingredients[#ingredients + 1] = tonumber(line)
    end
end

local fresh_ingredients = 0

for _, ingredient in ipairs(ingredients) do
    ingredient_is_fresh = false

    for _, range in ipairs(ranges) do
        if ingredient >= range['min'] and ingredient <= range['max'] then
            ingredient_is_fresh = true
        end
    end

    if ingredient_is_fresh then
        fresh_ingredients = fresh_ingredients + 1
    end
end

print("Part 1: " .. fresh_ingredients .. " fresh ingredients")

local range_limits = {}
local active_ranges = 0
local current_low_limit = 1
local fresh_ingredient_ids = 0

for _, range in ipairs(ranges) do
    if range_limits[range['min']] == nil then
        range_limits[range['min']] = {}
    end

    if range_limits[range['max']] == nil then
        range_limits[range['max']] = {}
    end

    range_limits[range['min']][#(range_limits[range['min']]) + 1] = 'min'
    range_limits[range['max']][#(range_limits[range['max']]) + 1] = 'max'
end

local table_keys = {}
local n = 0

for k, _ in pairs(range_limits) do
    n = n + 1
    table_keys[n] = k
end

table.sort(table_keys)

for _, key in ipairs(table_keys) do
    table.sort(range_limits[key], function(a, b) return a > b end)

    for _, limit_type in ipairs(range_limits[key]) do
        if limit_type == 'min' then
            if active_ranges == 0 then
                current_low_limit = key
            end

            active_ranges = active_ranges + 1
        end

        if limit_type == 'max' then
            active_ranges = active_ranges - 1

            if active_ranges == 0 then
                fresh_ingredient_ids = fresh_ingredient_ids + (key - current_low_limit + 1)
            end
        end
    end
end

print("Part 2: " .. fresh_ingredient_ids .. " fresh ingredient IDs")
