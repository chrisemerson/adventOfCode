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
