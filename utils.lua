local band = bit.band
local bor = bit.bor
local bxor = bit.bxor
local bnot = bit.bnot
local lshift = bit.lshift
local rshift = bit.rshift

UTILS = {}
local UTILS = UTILS

function UTILS.isDefined(v)
    return (v and v ~= CPU.UNDEFINED) and v or nil
end

function UTILS.bind(f, param)
    return function(...)
        return f(param, ...)
    end
end

function UTILS.timeF(f, n)
    local t = os.clock()
    for i = 1, n or 1 do
        f()
    end
    return os.clock() - t
end

function UTILS.tSetter(t, offs)
    offs = (offs or 0)
    return function(i, v)
        t[i + offs] = v
    end
end

function UTILS.tGetter(t, offs)
    offs = (offs or 1)
    return function(i)
        return t[i + offs]
    end
end

-- turns a table of tables, where the inner tables are all of a fixed size N
-- into a new table of tables, where the inner tables are all of size new_size
function UTILS.flattenSplit(tt, new_size)
    local tt_first_idx = tt[0] and 0 or 1

    local new_tt = {}
    local new_tt_idx = tt_first_idx
    local new_t_idx = tt[tt_first_idx][0] and 0 or 1

    if new_t_idx == 0 then
        new_size = new_size - 1
    end

    for tt_idx = tt_first_idx, #tt do
        local t = tt[tt_idx]
        for t_idx = t[0] and 0 or 1, #t do
            if new_t_idx == new_size + 1 then
                new_t_idx = tt[tt_first_idx][0] and 0 or 1
                new_tt_idx = new_tt_idx + 1
                new_tt[new_tt_idx] = {}
            elseif new_tt[new_tt_idx] == nil then
                new_tt[new_tt_idx] = {}
            end
            new_tt[new_tt_idx][new_t_idx] = t[t_idx]
            new_t_idx = new_t_idx + 1
        end
    end
    return new_tt
end

function UTILS.map(t, f)
    local tt = {}
    for i = t[0] and 0 or 1, #t do
        tt[i] = f(t[i], tt)
    end
    return tt
end

function UTILS.fill(t, v, n, step, offs)
    for i = t[0] and 0 or 1, math.max(#t, n or 0), step or 1 do
        t[i + (offs or 0)] = v
    end
    return t
end

function UTILS.shiftingArray()
    local _t = {}
    local t = {}
    local shift = 0
    setmetatable(
        t,
        {
            __index = function(t, k)
                return _t[UTILS.rotateIdx(_t, k + shift)]
            end,
            __newindex = function(t, k, v)
                if k > #_t then
                    table.insert(_t, shift + k - #_t - 1, v)
                else
                    _t[UTILS.rotateIdx(_t, k + shift)] = v
                end
            end
        }
    )
    t.rotate = function(self, newshift)
        shift = UTILS.rotateIdx(_t, newshift + shift)
    end
    return t
end

function UTILS.indexRotating(t, idx)
    return t[UTILS.rotateIdx(t, idx)]
end

function UTILS.rotatePositiveIdx(t, idx, size)
    size = size or #t
    return ((idx - 1) % size) + 1
end

function UTILS.rotateIdx(t, idx, size)
    size = size or #t
    if idx > size then
        return UTILS.rotateIdx(t, idx - size, size)
    elseif idx < 1 then
        return UTILS.rotateIdx(t, idx + size, size)
    else
        return idx
    end
end

-- In-place
function UTILS.rotate(array, shift) -- Works for array with consecutive entries
    shift = shift or 1              -- make second arg optional, defaults to 1

    local start = array[0] and 0 or 1
    local size = #array

    if shift > 0 then
        for i = 1, math.abs(shift) do
            table.insert(array, 1, table.remove(array, size))
        end
    else
        for i = 1, math.abs(shift) do
            table.insert(array, size, table.remove(array, 1))
        end
    end
    return array
end

function UTILS.rotateNew(t, r)
    local rotated = {}
    local size = #t
    local start = t[0] and 0 or 1
    if r >= 0 then
        for i = start, size do
            local idx = i + r
            if idx > size then
                idx = idx - size
            elseif (idx < start) then
                idx = idx + size
            end
            rotated[i] = t[idx]
        end
    else
        for i = 1, size do
            local idx = size - i + r
            if idx > size then
                idx = idx - size
            elseif (idx < start) then
                idx = idx + size
            end
            rotated[i] = t[idx]
        end
    end
    return rotated
end

function UTILS.nthBitIsSet(n, nth)
    return band(n, nth == 0 and 0x1 or lshift(0x1, nth)) ~= 0
end

function UTILS.nthBitIsSetInt(n, nth)
    return UTILS.nthBitIsSet(n, nth) and 1 or 0
end

function UTILS.transpose(t)
    local tt = {}
    if #t == 0 then
        return tt
    end
    local ttSize = #(t[1])
    for i = t[1][0] and 0 or 1, ttSize do
        local ttt = {}
        tt[i] = ttt
        for j = t[0] and 0 or 1, #t do
            ttt[j] = t[j][i]
        end
    end
    return tt
end

function UTILS.range(a, b, step)
    local t = {}
    -- is floor right here?
    local qty = (b - a)
    if not step then
        if qty > 0 then
            step = 1
        else
            step = -1
            qty = -qty
        end
    end
    for i = 0, (math.floor(math.abs(qty / step))) do
        t[i] = a + i * step
    end
    return t
end

function UTILS.printf(...)
    print(string.format(...))
end

function UTILS.concat0(...)
    local args = { ... }
    if type(args[1]) == "table" then
        local ct = {}
        for j = 1, #args do
            local t = args[j]
            for i = 0, #t do
                ct[(not ct[0]) and 0 or (#ct + 1)] = t[i]
            end
        end
        return ct
    else
        return table.concat(...)
    end
end

function UTILS.concat(...)
    local args = { ... }
    if type(args[1]) == "table" then
        local ct = {}
        for j = 1, #args do
            local t = args[j]
            for i = 1, #t do
                ct[#ct + 1] = t[i]
            end
        end
        return ct
    else
        return table.concat(...)
    end
end

function UTILS.copy(t, n, offset, step)
    local tt = {}
    n = n or #t
    offset = offset or 0
    for i = t[0] and 0 or 1, n, step or 1 do
        tt[i] = t[i + offset]
    end
    return tt
end

function UTILS.dump(o)
    if type(o) == "table" then
        local s = "{ "
        for k, v in pairs(o) do
            if type(k) ~= "number" then
                k = '"' .. k .. '"'
            end
            s = s .. "[" .. k .. "] = " .. UTILS.dump(v) .. ","
        end
        return s .. "} "
    else
        return tostring(o)
    end
end

function UTILS.dumpi(o)
    if type(o) == "table" then
        local s = "{ "
        for k, v in ipairs(o) do
            if type(k) ~= "number" then
                k = '"' .. k .. '"'
            end
            s = s .. UTILS.dumpi(v) .. ","
        end
        return s .. "} "
    else
        return tostring(o)
    end
end

function UTILS.all(t, f)
    for i = t[0] and 0 or 1, #t do
        if not f(t[i]) then
            return false
        end
    end
    return true
end

function UTILS.flat_map(t, f)
    t = UTILS.map(t, f)
    local tt = {}
    for j = t[0] and 0 or 1, #t do
        local st = t[j]
        for i = t[0] and 0 or 1, #st do
            local v = st[i]
            tt[#tt + 1] = v
        end
    end
    return tt
end

function UTILS.clear(t)
    for k in pairs(t) do
        t[k] = nil
    end
end

function UTILS.uniq(t)
    local tt = {}
    local done = {}
    for i = t[0] and 0 or 1, #t do
        local x = t[i]
        if not done[x] then
            tt[#tt + 1] = x
            done[x] = true
        end
    end
    return tt
end

local p = print
local f = nil
function UTILS.print(x)
    if not f then
        local ff = assert(io.open("logs.txt", "w"))
        ff:write("")
        ff:close()
        f = assert(io.open("logs.txt", "a"))
        asdasdsssasd = f
    end
    local str = UTILS.dump(x)
    f:write(str .. "\n")
    --f:flush()
    --p(str)
end

function UTILS.import(t)
    local e = getfenv(2)
    for k, v in pairs(t) do
        e[k] = v
    end
end

function UTILS.class(parent)
    local class = {}
    if parent then
        setmetatable(class, { __index = parent })
        class._parent = parent
    end
    class._mt = { __index = class }
    function class:new(...)
        local instance = {}
        setmetatable(instance, class._mt)
        if instance.initialize then
            instance:initialize(...)
        end
        return instance
    end

    return class
end

function UTILS.swapRanges(arrTable, start1, start2, size)
    for i = 1, size do
        local tmp = arrTable[i + start1]
        arrTable[i + start1] = arrTable[i + start2]
        arrTable[i + start2] = tmp
    end
end
