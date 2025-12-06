package.path = package.path .. ";../?.lua"
require "Util/table"

local input_file = "input.txt"

local columns = {}
local max_line_length = 1

for line in io.lines(input_file) do
    local cols_this_line = {}

    max_line_length = math.max(max_line_length, #line)

    for i = 1, #line do
        if string.sub(line, i, i) == " " then
            cols_this_line[#cols_this_line + 1] = i
        end
    end

    if #columns == 0 then
        columns = copy(cols_this_line)
    else
        local new_columns = {}

        for _, v in ipairs(cols_this_line) do
            if contains(columns, v) then
                new_columns[#new_columns + 1] = v
            end
        end

        columns = copy(new_columns)
    end
end

local prev_column_stop = 1
local data_p1 = {}

columns[#columns + 1] = max_line_length

for _, column_stop in ipairs(columns) do
    local this_data = {}

    for line in io.lines(input_file) do
        this_data[#this_data + 1] = string.gsub(string.sub(line, prev_column_stop, column_stop), "%s+", "")
    end

    prev_column_stop = column_stop
    data_p1[#data_p1 + 1] = this_data
end

local total = 0

for _, problem in ipairs(data_p1) do
    local operator = problem[#problem]
    local operands = {table.unpack(problem, 1, #problem - 1)}
    local this_problem

    if operator == '+' then
        this_problem = 0
    elseif operator == '*' then
        this_problem = 1
    end

    for _, operand in ipairs(operands) do
        if operator == '+' then
            this_problem = this_problem + operand
        elseif operator == '*' then
            this_problem = this_problem * operand
        end
    end

    total = total + this_problem
end

print("Part 1: " .. total)

local data_p2 = {}
local dataset = {}
local current_operator = ""

for i = max_line_length, 1, -1 do
    local current_number = ""

    for line in io.lines(input_file) do
        local current_char = string.sub(line, i, i)

        if current_char ~= " " and current_char ~= "+" and current_char ~= "*" then
            current_number = current_number .. current_char
        elseif current_char == "+" or current_char == "*" then
            current_operator = current_char
        end
    end

    if current_number == "" then
        data_p2[#data_p2 + 1] = { dataset, current_operator }
        current_operator = ""
        dataset = {}
    else
        dataset[#dataset + 1] = tonumber(current_number)
    end

    if i == 1 then
        data_p2[#data_p2 + 1] = { dataset, current_operator }
    end
end

local total_p2 = 0

for _, dataset in ipairs(data_p2) do
    local this_problem

    if dataset[2] == "+" then
        this_problem = 0
    elseif dataset[2] == "*" then
        this_problem = 1
    end

    for _, number in ipairs(dataset[1]) do
        if dataset[2] == "+" then
            this_problem = this_problem + number
        elseif dataset[2] == "*" then
            this_problem = this_problem * number
        end
    end

    total_p2 = total_p2 + this_problem
end

print("Part 2: " .. total_p2)
