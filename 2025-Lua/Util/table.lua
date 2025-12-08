function dump(o)
   if type(o) == 'table' then
      local s = '{ '
      for k,v in pairs(o) do
         if type(k) ~= 'number' then k = '"'..k..'"' end
         s = s .. '['..k..'] = ' .. dump(v) .. ','
      end
      return s .. '} '
   else
      return tostring(o)
   end
end

function copy(t)
    local new = {}

    for k, v in pairs(t) do
        if type(v) == "table" then
            new[k] = copy(v);
        else
            new[k] = v;
        end
    end

    return setmetatable(new, getmetatable(t))
end

function contains(haystack, needle)
    for _, v in ipairs(haystack) do
        if v == needle then
            return true
        end
    end

    return false
end

function slice(table, start, finish)
    local slice = {}

    for i = start, finish do
        slice[i] = table[i]
    end

    return slice
end

function keys(table)
    local keys = {}

    for k, _ in pairs(table) do
        keys[#keys + 1] = k
    end

    return keys
end

function key_exists(table, key)
    for k, _ in pairs(table) do
        if k == key then
            return true
        end
    end

    return false
end

function table_concat(a, b)
    local result = {}

    for _, v in ipairs(a) do
        result[#result + 1] = v
    end

    for _, v in ipairs(b) do
        result[#result + 1] = v
    end

    return result
end

function toset(table)
    local set = {}

    for _, v in ipairs(table) do
        set[v] = true
    end

    return set
end
