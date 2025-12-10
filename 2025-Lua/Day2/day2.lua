function is_repeating_number_p1(n)
    local l = string.len(n)
    return l % 2 == 0 and string.sub(n, 1, l / 2) == string.sub(n, l / 2 + 1)
end

function is_repeating_number_p2(n)
    local l = string.len(n)

    for lx = math.floor(l / 2), 1, -1 do
        if l % lx == 0 then
            local sequence
            local invalid_id = true

            for s = 1, l, lx do
                local sx = string.sub(n, s, s + lx - 1)

                if sequence == nil then
                    sequence = sx
                elseif sx ~= sequence then
                    invalid_id = false
                end
            end

            if invalid_id then
                return true
            end
        end
    end

    return false
end

local ranges = {}

for line in io.lines("input.txt") do
    for range in string.gmatch(line, "[^,]+") do
        _, _, min, max = string.find(range, "(%d+)%-(%d+)")
        ranges[#ranges + 1] = {['min'] = min, ['max'] = max}
    end
end

local total_p1 = 0
local total_p2 = 0

for _, range in ipairs(ranges) do
    for i = range['min'], range['max'], 1 do
        if is_repeating_number_p1(math.floor(i)) then
            total_p1 = total_p1 + i
        end

        if is_repeating_number_p2(math.floor(i)) then
            total_p2 = total_p2 + i
        end
    end
end

print("Part 1: ".. math.floor(total_p1))
print("Part 2: ".. math.floor(total_p2))
