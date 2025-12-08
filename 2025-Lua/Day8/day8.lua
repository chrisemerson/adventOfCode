package.path = package.path .. ";../?.lua"
require "Util/table"

local points = {}

local test = false
local input_file, connections_to_make

if test then
    input_file = "test.txt"
    connections_to_make = 10
else
    input_file = "input.txt"
    connections_to_make = 1000
end

for line in io.lines(input_file) do
    local _, _, x, y, z = string.find(line, "(%d+),(%d+),(%d+)")
    points[#points + 1] = {['x'] = tonumber(x), ['y'] = tonumber(y), ['z'] = tonumber(z)}
end

local distances = {}
local unique_distances = {}

for outeri, outerp in pairs(points) do
    for inneri, innerp in pairs(points) do
        if inneri ~= outeri then
            local dist_squared =
                (outerp['x'] - innerp['x']) ^ 2
                + (outerp['y'] - innerp['y']) ^ 2
                + (outerp['z'] - innerp['z']) ^ 2

            if distances[inneri] == nil then
                distances[inneri] = {}
            end

            if distances[outeri] == nil then
                distances[outeri] = {}
            end

            distances[inneri][outeri] = dist_squared
            distances[outeri][inneri] = dist_squared

            unique_distances[dist_squared] = true
        end
    end
end

unique_distances = keys(unique_distances)
table.sort(unique_distances)
local connections = {}

for _, dist in ipairs(slice(unique_distances, 1, connections_to_make)) do
    for i, neighbours in pairs(distances) do
        for o, dist_to_n in pairs(neighbours) do
            if dist_to_n == dist then
                if connections[i] == nil then
                    connections[i] = {}
                end

                if connections[o] == nil then
                    connections[o] = {}
                end

                connections[i][#connections[i] + 1] = o
                connections[o][#connections[o] + 1] = i
            end
        end
    end
end

function get_group(connections, node, already_connected)
    local group = { node }

    for _, conn in ipairs(connections[node]) do
        -- Only add if the connection isn't already added or it's already in the group
        if not (contains(already_connected, conn) or contains(group, conn)) then
            group[#group + 1] = conn

            for _, recur in ipairs(get_group(connections, conn, table_concat(group, already_connected))) do
                group[#group + 1] = recur
            end
        end
    end

    local set = toset(group)
    group = {}

    for k, _ in pairs(set) do
        group[#group + 1] = k
    end

    return group
end

local connection_groups = {}

for node, _ in pairs(connections) do
    local already_in_group = false

    for _, group_items in ipairs(connection_groups) do
        for _, group_item in ipairs(group_items) do
            if node == group_item then
                already_in_group = true
            end
        end
    end

    if not already_in_group then
        connection_groups[#connection_groups + 1] = get_group(connections, node, {})
    end
end

local group_sizes = {}

for _, group in ipairs(connection_groups) do
    group_sizes[#group_sizes + 1] = #group
end

table.sort(group_sizes, function(a, b) return a > b end)

local product = 1

for _, size in ipairs(slice(group_sizes, 1, 3)) do
    product = product * size
end

print("Part 1: " .. product)

connections = {}
local last_connection = {}

for _, dist in ipairs(unique_distances) do
    for i, neighbours in pairs(distances) do
        for o, dist_to_n in pairs(neighbours) do
            if dist_to_n == dist then
                if connections[i] == nil then
                    connections[i] = {}
                end

                if connections[o] == nil then
                    connections[o] = {}
                end

                connections[i][#connections[i] + 1] = o
                connections[o][#connections[o] + 1] = i

                last_connection = {points[o], points[i]}
            end
        end
    end

    connection_groups = {}

    for node, _ in pairs(connections) do
        local already_in_group = false

        for _, group_items in ipairs(connection_groups) do
            for _, group_item in ipairs(group_items) do
                if node == group_item then
                    already_in_group = true
                end
            end
        end

        if not already_in_group then
            connection_groups[#connection_groups + 1] = get_group(connections, node, {})
        end
    end

    if #connection_groups == 1 and #connection_groups[1] == #points then
        print("Part 2: " .. last_connection[1]['x'] * last_connection[2]['x'])
        os.exit()
    end
end
