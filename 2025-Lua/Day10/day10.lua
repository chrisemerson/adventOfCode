package.path = package.path .. ";../?.lua;../?/init.lua"
require "Util/table"

local alg = require 'sci/alg'

local wrap, yield = coroutine.wrap, coroutine.yield

--[[
    Yields combinations of non-repeating items of tbl.
    tbl is the source of items,
    sub is a combination of items that all yielded combination ought to contain,
    min it the minimum key of items that can be added to yielded combinations.
--]]
local function unique_combinations (tbl, sub, min)
    sub = sub or {}
    min = min or 1
    return wrap (function ()
        if #sub > 0 then
            yield (sub) -- yield short combination.
        end
        if #sub < #tbl then
            for i = min, #tbl do    -- iterate over longer combinations.
                for combo in unique_combinations (tbl, append (sub, tbl [i]), i + 1) do
                    yield (combo)
                end
            end
        end
    end)
end

function split_by_comma(str, offset)
    local items = {}

    for x in string.gmatch(str, "[^,]+") do
        items[#items + 1] = tonumber(x) + offset
    end

    return items
end

function parse_line(line)
    local _, _, lights, buttons, joltages = string.find(line, "%[([#%.]+)%]%s([%(%d%)%s,]+)%s%{([%d,]+)%}")

    local button_schematics = {}

    for button in string.gmatch(buttons, "%(([%d,]+)%)%s*") do
        button_schematics[#button_schematics + 1] = split_by_comma(button, 1)
    end

    return {['lights'] = lights, ['buttons'] = button_schematics, ['joltages'] = split_by_comma(joltages, 0)}
end

local machines = {}

for line in io.lines("test.txt") do
    machines[#machines + 1] = parse_line(line)
end

function apply_button(current_light_state, button)
    if string.sub(current_light_state, button, button) == "." then
        return string.sub(current_light_state, 0, button - 1) .. "#" .. string.sub(current_light_state, button + 1)
    elseif string.sub(current_light_state, button, button) == "#" then
        return string.sub(current_light_state, 0, button - 1) .. "." .. string.sub(current_light_state, button + 1)
    end

    return current_light_state
end

function apply_button_set(current_light_state, button_set)
    local this_light_state = current_light_state

    for _, button in ipairs(button_set) do
        this_light_state = apply_button(this_light_state, button)
    end

    return this_light_state
end

local total_button_presses = 0

for _, machine in ipairs(machines) do
    local min_button_presses_for_machine = nil

    for button_combo in unique_combinations(machine['buttons']) do
        local lights = string.rep(".", #machine['lights'])

        for _, button_set in ipairs(button_combo) do
            lights = apply_button_set(lights, button_set)
        end

        if lights == machine['lights'] then
            if min_button_presses_for_machine == nil then
                min_button_presses_for_machine = #button_combo
            elseif #button_combo < min_button_presses_for_machine then
                min_button_presses_for_machine = #button_combo
            end
        end
    end

    if min_button_presses_for_machine ~= nil then
        total_button_presses = total_button_presses + min_button_presses_for_machine
    end
end

print("Part 1: " .. total_button_presses)

local machine = machines[1]
local joltages = machine['joltages']
local joltage_state = {}

for _ = 1, #joltages do
    joltage_state[#joltage_state + 1] = 0
end

print(dump(joltage_state))

print(alg.mat(2,3))
