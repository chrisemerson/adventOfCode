function find_maximum_int_position_in_string(str)
    local max = 0
    local max_position = 0

    for i = 1, string.len(str), 1 do
        if tonumber(string.sub(str, i, i)) > max then
            max = tonumber(string.sub(str, i, i))
            max_position = i
        end
    end

    return max_position
end

function find_maximum_joltage(battery, len)
    local digits = {}
    local last_digit_pos = 0

    for i = 1, len, 1 do
        last_digit_pos = last_digit_pos + find_maximum_int_position_in_string(
            string.sub(battery, last_digit_pos + 1, string.len(battery) - (len - i + 1))
        )

        digits[#digits + 1] = string.sub(battery, last_digit_pos, last_digit_pos)
    end

    local final_string = ""

    for _, val in ipairs(digits) do
        final_string = final_string .. val
    end

    return tonumber(final_string)
end

local total_p1 = 0
local total_p2 = 0

for line in io.lines("input.txt") do
    total_p1 = total_p1 + find_maximum_joltage(line, 2)
    total_p2 = total_p2 + find_maximum_joltage(line, 12)
end

print("Part 1: ".. math.floor(total_p1))
print("Part 2: ".. math.floor(total_p2))
