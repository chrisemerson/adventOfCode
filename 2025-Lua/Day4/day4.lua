package.path = package.path .. ";../?.lua"

require "../Util/table"

function import_grid(file)
    local grid = {}

    for line in io.lines(file) do
        local row = {}

        for x = 1, #line do
            table.insert(row, string.sub(line, x, x))
        end

        table.insert(grid, row)
    end

    return grid
end

function cell_is_accessible(grid, y, x)
    local adjacent_rolls_of_paper = 0

    for dy = -1, 1 do
        for dx = -1, 1 do
            if
                y + dy >= 1 and y + dy <= #grid
            and x + dx >= 1 and x + dx <= #grid[1]
            and (dy ~= 0 or dx ~= 0)
            and grid[y + dy][x + dx] == '@' then
                adjacent_rolls_of_paper = adjacent_rolls_of_paper + 1
            end
        end
    end

    return adjacent_rolls_of_paper < 4
end

local grid = import_grid("input.txt")

local accessible_cells = 0
local rolls_of_paper_removed = 0
local rolls_of_paper_removed_this_loop
local is_first_loop = true

while rolls_of_paper_removed_this_loop == nil or rolls_of_paper_removed_this_loop > 0 do
    rolls_of_paper_removed_this_loop = 0
    local newgrid = copy(grid)

    for y, row in ipairs(grid) do
        for x, cell in ipairs(row) do
            if cell == '@' and cell_is_accessible(grid, y, x) then
                newgrid[y][x] = '.'
                rolls_of_paper_removed_this_loop = rolls_of_paper_removed_this_loop + 1
            end
        end
    end

    if is_first_loop then
        accessible_cells = rolls_of_paper_removed_this_loop
        is_first_loop = false
    end

    grid = copy(newgrid)
    rolls_of_paper_removed = rolls_of_paper_removed + rolls_of_paper_removed_this_loop
end

print("Part 1: " .. accessible_cells)
print("Part 2: " .. rolls_of_paper_removed)
