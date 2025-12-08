package.path = package.path .. ";../?.lua"
require "Util/table"

local start_pos = {}
local splitters = {}
local y = 0

for line in io.lines("input.txt") do
    y = y + 1

    for x = 1, #line do
        local char = string.sub(line, x, x)

        if char == 'S' then
            start_pos = { y, x }
        end

        if char == '^' then
            splitters[#splitters + 1] = { y, x }
        end
    end
end

local beams = {}
local timelines = {[start_pos[2]] = 1}
beams[start_pos[2]] = true

local splits = 0

for row = start_pos[1], y do
    local new_beams = {}
    local new_timelines = {}

    for beam, _ in pairs(beams) do
        local hit_splitter = false

        for _, splitter in ipairs(splitters) do
            if splitter[1] == row and splitter[2] == beam then
                hit_splitter = true
                splits = splits + 1
                new_beams[beam - 1] = true
                new_beams[beam + 1] = true
            end
        end

        if not hit_splitter then
            new_beams[beam] = true
        end
    end

    for x_pos, timeline_count in pairs(timelines) do
        local hit_splitter = false

        for _, splitter in ipairs(splitters) do
            if splitter[1] == row and splitter[2] == x_pos then
                hit_splitter = true
                if new_timelines[x_pos - 1] == nil then
                    new_timelines[x_pos - 1] = 0
                end

                if new_timelines[x_pos + 1] == nil then
                    new_timelines[x_pos + 1] = 0
                end

                new_timelines[x_pos - 1] = new_timelines[x_pos - 1] + timeline_count
                new_timelines[x_pos + 1] = new_timelines[x_pos + 1] + timeline_count
            end
        end

        if not hit_splitter then
            if new_timelines[x_pos] == nil then
                new_timelines[x_pos] = timeline_count
            else
                new_timelines[x_pos] = new_timelines[x_pos] + timeline_count
            end
        end
    end

    beams = new_beams
    timelines = new_timelines
end

local timeline_total = 0

for _, timeline_count in pairs(timelines) do
    timeline_total = timeline_total + timeline_count
end

print("Part 1: " .. splits)
print("Part 2: " .. timeline_total)
