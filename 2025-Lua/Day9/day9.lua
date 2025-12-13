package.path = package.path .. ";../?.lua"
require "Util/table"

local points = {}

for line in io.lines("input.txt") do
    local _, _, x, y = string.find(line, "(%d+),(%d+)")

    points[#points + 1] = {['x'] = tonumber(x), ['y'] = tonumber(y)}
end

local max_area = 0

for _, i in ipairs(points) do
    for _, j in ipairs(points) do
        local dx = math.abs(i['x'] - j['x']) + 1
        local dy = math.abs(i['y'] - j['y']) + 1

        if dx * dy > max_area then
            max_area = dx * dy
        end
    end
end

print("Part 1: " .. max_area)

local function line_intersects(a1, a2, b1, b2)
    if (a1['y'] == a2['y'] and b1['y'] == b2['y']) or (a1['x'] == a2['x'] and b1['x'] == b2['x']) then
        return false
    elseif (a1['y'] == a2['y']) then
        return b1['x'] > math.min(a1['x'], a2['x']) and b1['x'] < math.max(a1['x'], a2['x']) and a1['y'] > math.min(b1['y'], b2['y']) and a1['y'] < math.max(b1['y'], b2['y'])
    else
        return a1['x'] > math.min(b1['x'], b2['x']) and a1['x'] < math.max(b1['x'], b2['x']) and b1['y'] > math.min(a1['y'], a2['y']) and b1['y'] < math.max(a1['y'], a2['y'])
    end
end

local function point_is_on_line(point, line1, line2)
    if line1['y'] == line2['y'] then
        local left = {['y'] = line1['y'], ['x'] = math.min(line1['x'], line2['x'])}
        local right = {['y'] = line1['y'], ['x'] = math.max(line1['x'], line2['x'])}

        return point['y'] == line1['y'] and point['x'] > left['x'] and point['x'] < right['x']
    else
        local top = {['x'] = line1['x'], ['y'] = math.min(line1['y'], line2['y'])}
        local bottom = {['x'] = line1['x'], ['y'] = math.max(line1['y'], line2['y'])}

        return point['x'] == line1['x'] and point['y'] > top['y'] and point['y'] < bottom['y']
    end
end

local function rectangle_intersects(p1, p2, p3, p4)
    --Normalise the points
    local top_left = {['y'] = math.min(p1['y'], p2['y'], p3['y'], p4['y']), ['x'] = math.min(p1['x'], p2['x'], p3['x'], p4['x'])}
    local bottom_right = {['y'] = math.max(p1['y'], p2['y'], p3['y'], p4['y']), ['x'] = math.max(p1['x'], p2['x'], p3['x'], p4['x'])}
    local top_right = {['y'] = math.min(p1['y'], p2['y'], p3['y'], p4['y']), ['x'] = math.max(p1['x'], p2['x'], p3['x'], p4['x'])}
    local bottom_left = {['y'] = math.max(p1['y'], p2['y'], p3['y'], p4['y']), ['x'] = math.min(p1['x'], p2['x'], p3['x'], p4['x'])}

    --First, just check if there are any points fully inside the rectangle
    for _, p in ipairs(points) do
        if p['x'] > top_left['x'] and p['x'] < bottom_right['x'] and p['y'] > top_left['y'] and p['y'] < bottom_right['y'] then
            return true
        end
    end

    --Loop through line segments and check if there's an intersection with the rectangle
    local lp = points[#points]

    for _, p in ipairs(points) do
        -- Account for the case where the point is on a line and the next point is outside the shape but taking the line
        -- through the shape. Also check here for simple intersections

        if (point_is_on_line(lp, top_left, top_right) and p['y'] > top_left['y'])
        or (point_is_on_line(lp, bottom_left, bottom_right) and p['y'] < top_left['y'])
        or (point_is_on_line(lp, top_left, bottom_left) and p['x'] > top_left['x'])
        or (point_is_on_line(lp, top_right, bottom_right) and p['x'] < top_right['x'])
        or line_intersects(top_left, top_right, lp, p)
        or line_intersects(top_right, bottom_right, lp, p)
        or line_intersects(bottom_left, bottom_right, lp, p)
        or line_intersects(top_left, bottom_left, lp, p) then
            return true
        end

        lp = p
    end

    return false
end

max_area = 0

-- Loop through all possible rectangles between 2 points in the set
for _, i in ipairs(points) do
    for _, j in ipairs(points) do
        -- Calculate area
        local this_area = (math.abs(i['x'] - j['x']) + 1) * (math.abs(i['y'] - j['y']) + 1)

        -- If it's not bigger than the current biggest area we have, no point going through the expensive validation
        if this_area > max_area then
            --Work out the 2 other points in the rectangle
            local k = {['y'] = i['y'], ['x'] = j['x']}
            local l = {['y'] = j['y'], ['x'] = i['x']}

            if not rectangle_intersects(i, j, k, l) then
                max_area = this_area
            end
        end
    end
end

print("Part 2: " .. max_area)
