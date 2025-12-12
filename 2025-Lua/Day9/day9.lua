package.path = package.path .. ";../?.lua"
require "Util/table"

local points = {}
local left_bound = nil

for line in io.lines("input.txt") do
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

function point_in_polygon(points, y, x)
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

function line_intersects(ax1, ay1, ax2, ay2, bx1, by1, bx2, by2)
    -- a is our horizontal line, b our vertical line

    -- Vertical line is entirely above or below our horizontal one
    if (by1 <= ay1 and by2 <= ay2) or (by1 > ay1 and by2 > ay2) then
        --print("Do not intersect")
        return false
    end

    -- Horizontal line is entirely to the left of our vertical line
    if (ax1 < bx1 and ax2 < bx2) then
        --print("Do not intersect")
        return false
    end

    --print("Intersects")
    return true
end

max_area = 0

for _, i in ipairs(points) do
    for _, j in ipairs(points) do
        local this_area = (math.abs(i['x'] - j['x']) + 1) * (math.abs(i['y'] - j['y']) + 1)
        local all_points_inside = true

        for ddy = math.min(i['y'], j['y']), math.max(i['y'], j['y']) do
            for ddx = math.min(i['x'], j['x']), math.max(i['x'], j['x']) do
                if not point_in_polygon(points, ddy, ddx) then
                    all_points_inside = false
                end
            end
        end

        if this_area > max_area and all_points_inside then
            max_area = this_area
        end
    end
end

print("Part 2: " .. max_area)
