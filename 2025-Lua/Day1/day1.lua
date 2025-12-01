local lines = {}

for line in io.lines("input.txt") do
    lines[#lines + 1] = line
end

local dial_pos = 50
local zero_count_1 = 0
local zero_count_2 = 0

for k,v in pairs(lines) do
    local n = tonumber(string.sub(v, 2))
    local minimal_n = n % 100
    local complete_turns = (n - minimal_n) / 100

    zero_count_2 = zero_count_2 + complete_turns

    if string.sub(v, 1, 1) == "R" then
        if (100 - dial_pos) <= minimal_n and dial_pos ~= 0 then
            zero_count_2 = zero_count_2 + 1
        end

        dial_pos = (dial_pos + minimal_n) % 100
    else
        if dial_pos <= minimal_n and dial_pos ~= 0 then
            zero_count_2 = zero_count_2 + 1
        end

        dial_pos = (dial_pos + (100 - minimal_n)) % 100
    end

    if dial_pos == 0 then
        zero_count_1 = zero_count_1 + 1
    end
end

print("Part 1: "..zero_count_1)
print("Part 2: "..math.floor(zero_count_2))