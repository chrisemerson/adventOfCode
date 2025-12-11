package.path = package.path .. ";../?.lua;../?/init.lua"
require "Util/table"

local graph = {}

for line in io.lines("input.txt") do
    local _, _, node, output_string = string.find(line, "([%a]+)%:%s*([%a%s]*)")

    local outputs = {}

    for output in string.gmatch(output_string, "([%a]+)") do
        outputs[#outputs + 1] = output
    end

    graph[node] = outputs
end

function count_paths_p1(network, node)
    if node == "out" then
        return 1
    end

    local paths_to_goal = 0

    for _, next_node in ipairs(network[node]) do
        paths_to_goal = paths_to_goal + count_paths_p1(network, next_node)
    end

    return paths_to_goal
end

local memoized = {}

function count_paths_p2(network, node, visited_dac, visited_fft)
    if node == "out" and visited_dac and visited_fft then
        return 1
    elseif node == "out" then
        return 0
    elseif memoized[node] ~= nil and memoized[node][visited_dac] ~= nil and memoized[node][visited_dac][visited_fft] ~= nil then
        return memoized[node][visited_dac][visited_fft]
    end

    local paths_to_goal = 0
    local this_visited_fft = visited_fft
    local this_visited_dac = visited_dac

    if node == 'dac' then this_visited_dac = true end
    if node == 'fft' then this_visited_fft = true end

    for _, next_node in ipairs(network[node]) do
        paths_to_goal = paths_to_goal + count_paths_p2(network, next_node, this_visited_dac, this_visited_fft)
    end

    if memoized[node] == nil then
        memoized[node] = {}
    end

    if memoized[node][visited_dac] == nil then
        memoized[node][visited_dac] = {}
    end

    memoized[node][visited_dac][visited_fft] = paths_to_goal

    return paths_to_goal
end

print("Part 1: " .. count_paths_p1(graph, "you"))
print("Part 2: " .. count_paths_p2(graph, "svr", false, false))
