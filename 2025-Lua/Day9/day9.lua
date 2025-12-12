package.path = package.path .. ";../?.lua"
require "Util/table"

local points = {}
local left_bound = nil

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

        left_bound = math.min(left_bound, i['x'], j['x'])

        local dx = math.abs(i['x'] - j['x']) + 1
        local dy = math.abs(i['y'] - j['y']) + 1

        if dx * dy > max_area then
            max_area = dx * dy
        end
    end
end

print("Part 1: " .. max_area)

left_bound = left_bound - 1

local function point_in_polygon(points, y, x)
    local last_point = points[#points]
    local in_polygon = false

    for _, point in ipairs(points) do
        if last_point['x'] == point['x'] then
            if line_intersects(left_bound, y, x, y, last_point['x'], last_point['y'], point['x'], point['y']) then
                in_polygon = not in_polygon
            end
        elseif last_point['y'] == point['y'] then
            if y == last_point['y'] and y >= math.min(last_point['y'], point['y']) and y <= math.max(last_point['y'], point['y']) then
                return true
            end
        end

        last_point = point
    end

    return in_polygon
end

local function line_intersects(ax1, ay1, ax2, ay2, bx1, by1, bx2, by2)
    --for y = math.min(ay1, ay2, by1, by2) - 1, math.max(ay1, ay2, by1, by2) + 1 do
    --    for x = math.min(ax1, ax2, bx1, bx2) - 1, math.max(ax1, ax2, bx1, bx2) + 1 do
    --        local a_vertical = ax1 == ax2
    --        local b_vertical = bx1 == bx2
    --
    --        local point_on_a = (a_vertical and ax1 == x and math.min(ay1, ay2) <= y and math.max(ay1, ay2) >= y) or (not a_vertical and ay1 == y and math.min(ax1, ax2) <= x and math.max(ax1, ax2) >= x)
    --        local point_on_b = (b_vertical and bx1 == x and math.min(by1, by2) <= y and math.max(by1, by2) >= y) or (not b_vertical and by1 == y and math.min(bx1, bx2) <= x and math.max(bx1, bx2) >= x)
    --
    --        if point_on_a and point_on_b then
    --            if a_vertical and b_vertical then
    --                io.write('H')
    --            elseif a_vertical or b_vertical then
    --                io.write('+')
    --            else
    --                io.write('=')
    --            end
    --        elseif point_on_a then
    --            if a_vertical then
    --                io.write('|')
    --            else
    --                io.write('-')
    --            end
    --        elseif point_on_b then
    --            if b_vertical then
    --                io.write('|')
    --            else
    --                io.write('-')
    --            end
    --        else
    --            io.write(".")
    --        end
    --    end
    --
    --    print()
    --end

    --Are both lines horizontal?
    if ay1 == ay2 and by1 == by2 then
        --If they have different y values, they don't intersect
        if ay1 ~= by1 then return false end

        --Check for overlap
        return not ((ax1 < math.min(bx1, bx2) and ax2 < math.min(bx1, bx2)) or (ax1 > math.max(bx1, bx2) and ax2 > math.max(bx1, bx2)))
    end

    --Are both lines vertical?
    if ax1 == ax2 and bx1 == bx2 then
        --If they have different x values, they don't intersect
        if ax1 ~= bx1 then return false end

        --Check for overlap
        return not ((ay1 < math.min(by1, by2) and ay2 < math.min(by1, by2)) or (ay1 > math.max(by1, by2) and ay2 > math.max(by1, by2)))
    end

    --One line is vertical and the other horizontal
    if ay1 == ay2 then
        --A is the horizontal line
        --First check if B is off to the left or right of the entirety of A
        if (bx1 < ax1 and bx1 < ax2) or (bx1 > ax1 and bx1 > ax2) then return false end

        --Check if B is entirely above or below A
        if (by1 < ay1 and by2 < ay1) or (by1 > ay1 and by2 > ay1) then return false end
    else
        --B is the horizontal line
        --First check if B is off to the left or right of  A
        if (bx1 < ax1 and bx2 < ax1) or (bx1 > ax1 and bx2 > ax1) then return false end

        --Check if B is entirely above or below A
        if (by1 < ay1 and by1 < ay2) or (by1 > ay1 and by1 > ay2) then return false end
    end

    return true
end

max_area = 0

for _, i in ipairs(points) do
    for _, j in ipairs(points) do
        local this_area = (math.abs(i['x'] - j['x']) + 1) * (math.abs(i['y'] - j['y']) + 1)
        local topleft = {['y'] = math.min(i['y'], j['y']), ['x'] = math.min(i['x'], j['x'])}
        local bottomright = {['y'] = math.max(i['y'], j['y']), ['x'] = math.max(i['x'], j['x'])}

        local last_point = points[#points]
        local in_polygon = true

        local output = this_area == 24

        if output then
            print("Rectangle from " .. topleft['y'] .. "," .. topleft['x'] .. " to " .. bottomright['y'] .. "," .. bottomright['x'])
        end

        for _, point in ipairs(points) do
            local line_string = last_point['y'] .. "," .. last_point['x'] .. " " .. point['y'] .. "," .. point['x']

            -- Left edge
            if line_intersects(last_point['x'], last_point['y'], point['x'], point['y'], topleft['x'], topleft['y'] + 1, topleft['x'], bottomright['y'] - 1) then
                if output then
                    print("left edge intersects with line " .. line_string)
                end

                in_polygon = false
            end

            -- Top edge
            if line_intersects(last_point['x'], last_point['y'], point['x'], point['y'], topleft['x'] + 1, topleft['y'], bottomright['x'] - 1, topleft['y']) then
                if output then
                print("top edge intersects with line " .. line_string)
                end
                in_polygon = false
            end

            -- Right edge
            if line_intersects(last_point['x'], last_point['y'], point['x'], point['y'], bottomright['x'], topleft['y'] + 1, bottomright['x'], bottomright['y'] - 1) then
                if output then
                print("right edge intersects with line " .. line_string)
                end
                in_polygon = false
            end

            -- Bottom edge
            if line_intersects(last_point['x'], last_point['y'], point['x'], point['y'], topleft['x'] + 1, bottomright['y'], bottomright['x'] - 1, bottomright['y']) then
                if output then
                print("bottom edge intersects with line " .. line_string)
                end
                in_polygon = false
            end
            --
            --print()
            --print()

            last_point = point
        end

                if output then
        print(this_area)
        print(in_polygon)
end
        if this_area > max_area and in_polygon then
            print("Rectangle " .. dump(topleft) .. " " .. dump(bottomright) .. " has max area " .. this_area)
            max_area = this_area
        end
    end
end

print("Part 2: " .. max_area)
