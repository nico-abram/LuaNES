--[[
  Thiss aims to "cache" unpack (Maybe also "cache" self and op indexing?)
]]
CPU.DISPATCHER = {}
for k, v in pairs(CPU.DISPATCH) do
  local op = v[1]
  local cacheF
  if #v == 2 then
    local a = unpack(v, 2)
    cacheF = function(self)
      self[op](self, a)
    end
  elseif #v == 1 then
    cacheF = function(self)
      self[op](self)
    end
  elseif #v == 3 then
    local a, b = unpack(v, 2)
    cacheF = function(self)
      self[op](self, a, b)
    end
  elseif #v == 4 then
    local a, b, c = unpack(v, 2)
    cacheF = function(self)
      self[op](self, a, b, c)
    end
  elseif #v == 5 then
    local a, b, c, d = unpack(v, 2)
    cacheF = function(self)
      self[op](self, a, b, c, d)
    end
  end
  CPU.DISPATCHER[k] = cacheF
end

return CPU
