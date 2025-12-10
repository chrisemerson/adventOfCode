package.path = package.path .. ";../?.lua"
require "Util/table"

function split_by_comma(str)
    local items = {}

    for x in string.gmatch(str, "[^,]+") do
        items[#items + 1] = tonumber(x) + 1
    end

    return items
end

function parse_line(line)
    local _, _, lights, buttons, joltages = string.find(line, "%[([#%.]+)%]%s([%(%d%)%s,]+)%s%{([%d,]+)%}")

    local button_schematics = {}

    for button in string.gmatch(buttons, "%(([%d,]+)%)%s*") do
        button_schematics[#button_schematics + 1] = split_by_comma(button)
    end

    return {['lights'] = lights, ['buttons'] = button_schematics, ['joltages'] = split_by_comma(joltages)}
end

local machines = {}

for line in io.lines("input.txt") do
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

function find_minimum_button_presses(lights, buttons, current_light_state)
    if lights == current_light_state then
        return {['presses'] = 0, ['buttons'] = {}}
    end

    if #buttons == 0 then
        return nil
    end

    local min_button_presses
    local minimal_button_set = {}

    for i, button_set in pairs(buttons) do
        local this_light_state = apply_button_set(current_light_state, button_set)
        local new_buttons = {}

        for j, jbutton_set in pairs(buttons) do
            if i ~= j then
                new_buttons[j] = jbutton_set
            end
        end

        local min_button_presses_from_here = find_minimum_button_presses(
            lights,
            new_buttons,
            this_light_state
        )

        if min_button_presses_from_here ~= nil then
            if min_button_presses == nil then
                min_button_presses = min_button_presses_from_here['presses']
                minimal_button_set = table_concat(min_button_presses_from_here['buttons'], button_set)
            elseif min_button_presses_from_here['presses'] < min_button_presses then
                min_button_presses = min_button_presses_from_here['presses']
                minimal_button_set = table_concat(min_button_presses_from_here['buttons'], button_set)
            end
        end
    end

    if min_button_presses == nil then
        return nil
    else
        return {['presses'] = min_button_presses + 1, ['buttons'] = minimal_button_set}
    end
end

local total_button_presses = 0

for _, machine in ipairs(machines) do
    total_button_presses = total_button_presses + find_minimum_button_presses(
        machine['lights'],
        machine['buttons'],
        string.rep(".", #machine['lights'])
    )['presses']
end

print("Part 1: " .. total_button_presses)
