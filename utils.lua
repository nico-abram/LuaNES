UTILS = {}
local UTILS = UTILS

function UTILS.isDefined(v)
    return v and v ~= CPU.UNDEFINED and v or nil
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

function UTILS.tSetter(t)
    return function(i, v)
        t[i] = v
    end
end
function UTILS.tGetter(t, offs)
    return function(i)
        return t[i + (offs or 1)]
    end
end
function UTILS.map(t, f)
    local tt = {}
    for i = t[0] and 0 or 1, #t do
        tt[i] = f(t[i])
    end
    return tt
end
function UTILS.fill(t, v, n, step, offs)
    for i = t[0] and 0 or 1, math.max(#t, n or 0), step or 1 do
        t[i + (offs or 0)] = v
    end
    return t
end
function UTILS.rotate(t, r)
    local rotated = {}
    local size = #t
    for i = t[0] and 0 or 1, size do
        local idx = i + r
        idx = idx > size and idx - size or (idx < 1 and idx + size or idx)
        rotated[i] = t[idx]
    end
    return rotated
end
function UTILS.nthBitIsSet(n, nth)
    return bit.band(n, bit.lshift(0x1, nth)) ~= 0
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
    for i = 0, b - a + 1, step or 1 do
        t[i] = a + i
    end
    return t
end
function UTILS.printf(...)
    print(string.format(...))
end
function UTILS.concat(...)
    local args = {...}
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
local asdasdasd
function UTILS.print(x)
    if not f then
        local ff = assert(io.open("logs.txt", "w"))
        ff:write("")
        ff:close()
        f = assert(io.open("logs.txt", "a"))
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
        setmetatable(class, {__index = parent})
        class._parent = parent
    end
    class._mt = {__index = class}
    function class:new(...)
        local instance = {}
        setmetatable(instance, class._mt)
        if instance.initialize then
            print "initC"
            instance:initialize(...)
        end
        return instance
    end
    return class
end
