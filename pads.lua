local band, bor, bxor, bnot, lshift, rshift = bit.band, bit.bor, bit.bxor, bit.bnot, bit.lshift, bit.rshift
local map, rotatePositiveIdx, nthBitIsSet, nthBitIsSetInt =
  UTILS.map,
  UTILS.rotatePositiveIdx,
  UTILS.nthBitIsSet,
  UTILS.nthBitIsSetInt

Pads = {}
local Pads = Pads
Pads._mt = {__index = Pads}
function Pads:new(conf, cpu, apu)
  local pads = {}
  setmetatable(pads, Pads._mt)
  pads:initialize(conf, cpu, apu)
  return pads
end
function Pads:initialize(conf, cpu, apu)
  self.conf = conf
  self.cpu = cpu
  self.apu = apu
  self.pads = {Pad:new(), Pad:new()}
end

function Pads:reset()
  self.cpu:add_mappings(0x4016, UTILS.bind(self.peek_401x, self), UTILS.bind(self.poke_4016, self))
  self.cpu:add_mappings(0x4017, UTILS.bind(self.peek_401x, self), UTILS.bind(self.apu.poke_4017, self.apu)) -- delegate 4017H to APU
  self.pads[1]:reset()
  self.pads[2]:reset()
end

function Pads:peek_401x(addr)
  self.cpu:update()
  return bor(self.pads[addr - 0x4016 + 1]:peek(), 0x40)
end

function Pads:poke_4016(_addr, data)
  self.pads[1]:poke(data)
  self.pads[2]:poke(data)
end

-- APIs

function Pads:keydown(pad, btn)
  self.pads[pad].buttons = bor(self.pads[pad].buttons, lshift(1, btn))
end

function Pads:keyup(pad, btn)
  self.pads[pad].buttons = band(self.pads[pad].buttons, bnot(lshift(1, btn)))
end

-- each pad
Pad = UTILS.class()
Pad.A = 0
Pad.B = 1
Pad.SELECT = 2
Pad.START = 3
Pad.UP = 4
Pad.DOWN = 5
Pad.LEFT = 6
Pad.RIGHT = 7

function Pad:initialize()
  self:reset()
end

function Pad:reset()
  self.strobe = false
  self.buttons = 0
  self.stream = 0
end

function Pad:poke(data)
  local prev = self.strobe
  self.strobe = nthBitIsSetInt(data, 0) == 1
  if prev and not self.strobe then
    self.stream = bxor(lshift(self:poll_state(), 1), -512)
  end
end

function Pad:peek()
  if self.strobe then
    return band(self:poll_state(), 1)
  end
  self.stream = rshift(self.stream, 1)
  return nthBitIsSetInt(self.stream, 0)
end

function Pad:poll_state()
  local state = self.buttons

  -- prohibit impossible simultaneous keydown (right and left, up and down)
  -- 0b00110000
  if band(state, 0x30) == 0x30 then
    state = band(state, 0xff - 0x30)
  end
  --0b00111111
  if band(state, 0xc0) == 0xc0 then
    state = band(state, 0xff - 0xc0)
  end

  return state
end
