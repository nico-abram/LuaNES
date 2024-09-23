-- Refer to https://wiki.nesdev.com/w/index.php/APU
-- for documentation on how the APU works
require "table.clear"
local band, bor, bxor, bnot, lshift, rshift = bit.band, bit.bor, bit.bxor, bit.bnot, bit.lshift, bit.rshift
local map, rotatePositiveIdx, nthBitIsSet, nthBitIsSetInt, range, concat0, concat =
    UTILS.map,
    UTILS.rotatePositiveIdx,
    UTILS.nthBitIsSet,
    UTILS.nthBitIsSetInt,
    UTILS.range,
    UTILS.concat0,
    UTILS.concat

APU = {}
local APU = APU
APU._mt = { __index = APU }

local NES =
    Nes or
    {
      -- DUMMY NES
      RP2A03_CC = 12,
      FOREVER_CLOCK = 0xffffffff
    }
APU.CLK_M2_MUL = 6
APU.CLK_NTSC = 39375000 * APU.CLK_M2_MUL
APU.CLK_NTSC_DIV = 11

APU.CHANNEL_OUTPUT_MUL = 256
APU.CHANNEL_OUTPUT_DECAY = APU.CHANNEL_OUTPUT_MUL / 4 - 1

APU.FRAME_CLOCKS =
    UTILS.map(
      { 29830, 1, 1, 29828 },
      function(n)
        return CPU.RP2A03_CC * n
      end
    )
APU.OSCILLATOR_CLOCKS =
    map(
      {
        { 7458, 7456, 7458, 7458 },
        { 7458, 7456, 7458, 7458 + 7452 }
      },
      function(a)
        return UTILS.map(
          a,
          function(n)
            return CPU.RP2A03_CC * n
          end
        )
      end
    )
function APU:new(conf, cpu, rate, bits)
  local apu = {}
  setmetatable(apu, APU._mt)
  apu:initialize(conf, cpu, rate, bits)
  return apu
end

function APU:initialize(conf, cpu, rate, bits)
  self.conf = conf
  self.cpu = cpu
  rate = rate or 44100
  bits = bits or 8

  self.pulse_0, self.pulse_1 = Pulse:new(self), Pulse:new(self)
  self.triangle = Triangle:new(self)
  self.noise = Noise:new(self)
  self.dmc = DMC:new(self.cpu, self)
  self.mixer = MIXER:new(self.pulse_0, self.pulse_1, self.triangle, self.noise, self.dmc)

  if rate < 11050 then
    error("audio sample rate must be >= 11050")
  end
  if bits ~= 8 and bits ~= 16 then
    error("audio bit depth must be 8 or 16")
  end

  self.settings_rate = rate

  self.output = {}
  self.buffer = {}

  self.fixed_clock = 1
  self.rate_clock = 1
  self.rate_counter = 0
  self.frame_counter = 0
  self.frame_divider = 0
  self.frame_irq_clock = 0
  self.frame_irq_repeat = 0
  self.dmc_clock = 0

  self:reset(false)
  return self
end

function APU:reset_mapping()
  self.frame_counter = self.frame_counter / self.fixed_clock
  self.rate_counter = self.rate_counter / self.fixed_clock
  local multiplier = 0
  while true do
    multiplier = multiplier + 1
    if multiplier >= 512 then
      break
    end
    if APU.CLK_NTSC * multiplier % self.settings_rate == 0 then
      break
    end
  end
  self.rate_clock = APU.CLK_NTSC * multiplier / self.settings_rate
  self.fixed_clock = APU.CLK_NTSC_DIV * multiplier
  self.frame_counter = self.frame_counter * self.fixed_clock
  self.rate_counter = self.rate_counter * self.fixed_clock

  self.mixer:reset()
  self.buffer = {} -- is this right? should we empty existing table? (It was .clear())

  local multiplier = 0
  while true do
    multiplier = multiplier + 1
    if multiplier >= 0x1000 then
      break
    end
    if APU.CLK_NTSC * (multiplier + 1) / self.settings_rate > 0x7ffff then
      break
    end
    if APU.CLK_NTSC * multiplier % self.settings_rate == 0 then
      break
    end
  end
  local rate = APU.CLK_NTSC * multiplier / self.settings_rate
  local fixed = APU.CLK_NTSC_DIV * CPU.CLK[1] * multiplier

  self.pulse_0:update_settings(rate, fixed)
  self.pulse_1:update_settings(rate, fixed)
  self.triangle:update_settings(rate, fixed)
  self.noise:update_settings(rate, fixed)

  self.cpu:add_mappings(0x4000, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.pulse_0.poke_0, self.pulse_0))
  self.cpu:add_mappings(0x4001, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.pulse_0.poke_1, self.pulse_0))
  self.cpu:add_mappings(0x4002, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.pulse_0.poke_2, self.pulse_0))
  self.cpu:add_mappings(0x4003, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.pulse_0.poke_3, self.pulse_0))
  self.cpu:add_mappings(0x4004, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.pulse_1.poke_0, self.pulse_1))
  self.cpu:add_mappings(0x4005, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.pulse_1.poke_1, self.pulse_1))
  self.cpu:add_mappings(0x4006, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.pulse_1.poke_2, self.pulse_1))
  self.cpu:add_mappings(0x4007, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.pulse_1.poke_3, self.pulse_1))
  self.cpu:add_mappings(0x4008, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.triangle.poke_0, self.triangle))
  self.cpu:add_mappings(0x400a, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.triangle.poke_2, self.triangle))
  self.cpu:add_mappings(0x400b, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.triangle.poke_3, self.triangle))
  self.cpu:add_mappings(0x400c, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.noise.poke_0, self.noise))
  self.cpu:add_mappings(0x400e, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.noise.poke_2, self.noise))
  self.cpu:add_mappings(0x400f, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.noise.poke_3, self.noise))
  self.cpu:add_mappings(0x4010, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.dmc.poke_0, self.dmc))
  self.cpu:add_mappings(0x4011, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.dmc.poke_1, self.dmc))
  self.cpu:add_mappings(0x4012, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.dmc.poke_2, self.dmc))
  self.cpu:add_mappings(0x4013, UTILS.bind(self.peek_40xx, self), UTILS.bind(self.dmc.poke_3, self.dmc))
  self.cpu:add_mappings(0x4015, UTILS.bind(self.peek_4015, self), UTILS.bind(self.poke_4015, self))
  self.frame_irq_clock = (self.frame_counter / self.fixed_clock) - CPU.CLK[1]
end

function APU:reset(mapping)
  mapping = mapping == nil and true or mapping
  self.cycles_ratecounter = 0
  self.frame_divider = 0
  self.frame_irq_clock = CPU.FOREVER_CLOCK
  self.frame_irq_repeat = 0
  self.dmc_clock = DMC.LUT[1]
  self.frame_counter = APU.FRAME_CLOCKS[1] * self.fixed_clock

  if mapping then
    self:reset_mapping()
  end

  self.pulse_0:reset()
  self.pulse_1:reset()
  self.triangle:reset()
  self.noise:reset()
  self.dmc:reset()
  self.mixer:reset()
  self.buffer = {} -- ..clear -> this. Is it enough? (Or do we empty existing table)
  self.oscillator_clocks = APU.OSCILLATOR_CLOCKS[1]
end

--##########################################################################
-- other APIs

function APU:do_clock()
  local curClk = self.cpu:current_clock()
  self:clock_dma(curClk)
  curClk = self.cpu:current_clock()
  if self.frame_irq_clock <= curClk then
    self:clock_frame_irq(curClk)
  end
  return math.min(self.dmc_clock, self.frame_irq_clock)
end

function APU:clock_dma(clk)
  if self.dmc_clock <= clk then
    self:clock_dmc(clk)
  end
end

function APU:update(target)
  target = (target or self.cpu:update()) * self.fixed_clock
  self:proceed(target)
  if self.frame_counter < target then
    return self:clock_frame_counter()
  end
end

function APU:update_latency()
  self:update(self.cpu:update() + 1)
end

function APU:update_delta()
  local elapsed = self.cpu:update()
  local delta = self.frame_counter ~= elapsed * self.fixed_clock
  self:update(elapsed + 1)
  return delta
end

function APU:vsync()
  self:flush_sound()
  self:update(self.cpu:current_clock())
  local frame = self.cpu:next_frame_clock()
  self.dmc_clock = self.dmc_clock - frame
  if self.frame_irq_clock ~= CPU.FOREVER_CLOCK then
    self.frame_irq_clock = self.frame_irq_clock - frame
  end
  frame = frame * self.fixed_clock
  self.rate_counter = self.rate_counter - frame
  self.frame_counter = self.frame_counter - frame
end

-- helpers

function APU:clock_oscillators(two_clocks)
  self.pulse_0:clock_envelope()
  self.pulse_1:clock_envelope()
  self.triangle:clock_linear_counter()
  self.noise:clock_envelope()
  if not two_clocks then
    return
  end
  self.pulse_0:clock_sweep(-1)
  self.pulse_1:clock_sweep(0)
  self.triangle:clock_length_counter()
  self.noise:clock_length_counter()
end

function APU:clock_dmc(target)
  repeat
    if self.dmc.clock_dac then
      self:update(self.dmc_clock)
      self.dmc:update()
    end
    self.dmc_clock = self.dmc_clock + self.dmc.freq
    self.dmc:clock_dma() -- TODO: IS THIS RIGHT?
  until not (self.dmc_clock <= target)
end

function APU:clock_frame_counter()
  self:clock_oscillators(nthBitIsSetInt(self.frame_divider, 0) == 1)
  self.frame_divider = band((self.frame_divider + 1), 3)
  self.frame_counter = self.frame_counter + self.oscillator_clocks[self.frame_divider + 1] * self.fixed_clock
end

function APU:clock_frame_irq(target)
  self.cpu:do_irq(CPU.IRQ_FRAME, self.frame_irq_clock)
  repeat
    self.frame_irq_clock = self.frame_irq_clock + APU.FRAME_CLOCKS[2 + self.frame_irq_repeat % 3]
    self.frame_irq_repeat = self.frame_irq_repeat + 1
  until not (self.frame_irq_clock <= target)
end

function APU:flush_sound()
  if #self.buffer < self.settings_rate / 60 then
    local target = self.cpu:current_clock() * self.fixed_clock
    self:proceed(target)
    if #self.buffer < self.settings_rate / 60 then
      if self.frame_counter < target then
        self:clock_frame_counter()
      end
      while #self.buffer < self.settings_rate / 60 do
        self.buffer[#(self.buffer) + 1] = self.mixer:sample()
      end
    end
  end
  self.output = self.buffer --  concat({}, self.buffer)
  self.buffer = {}          --.clear
  --table.clear(self.buffer)
end

function APU:proceed(target)
  while self.rate_counter < target and #self.buffer < self.settings_rate / 60 do
    self.buffer[#(self.buffer) + 1] = self.mixer:sample()
    if self.frame_counter <= self.rate_counter then
      self:clock_frame_counter()
    end
    self.rate_counter = self.rate_counter + self.rate_clock
  end
end

-- mapped memory handlers

-- Control
function APU:poke_4015(_addr, data)
  self:update()
  self.pulse_0:enable(nthBitIsSet(data, 0))
  self.pulse_1:enable(nthBitIsSet(data, 1))
  self.triangle:enable(nthBitIsSet(data, 2))
  self.noise:enable(nthBitIsSet(data, 3))
  self.dmc:enable(nthBitIsSet(data, 4))
end

-- Status
--function
function APU:peek_4015(_addr)
  local elapsed = self.cpu:update()
  if self.frame_irq_clock <= elapsed then
    self:clock_frame_irq(elapsed)
  end
  if self.frame_counter < elapsed * self.fixed_clock then
    self:update(elapsed)
  end
  return bor(
    self.cpu:clear_irq(CPU.IRQ_FRAME),
    (self.pulse_0.status and 0x01 or 0),
    (self.pulse_1.status and 0x02 or 0),
    (self.triangle.status and 0x04 or 0),
    (self.noise.status and 0x08 or 0),
    (self.dmc.status and 0x10 or 0)
  )
end

-- Frame counter (NOTE: this handler is called via Pads)
function APU:poke_4017(_addr, data)
  local n = self.cpu:update()
  if self.cpu:odd_clock() then
    n = n + CPU.CLK[1]
  end
  self:update(n)
  if self.frame_irq_clock <= n then
    clock_frame_irq(n)
  end
  n = n + CPU.CLK[1]
  self.oscillator_clocks = APU.OSCILLATOR_CLOCKS[nthBitIsSetInt(data, 7) + 1]
  self.frame_counter = (n + self.oscillator_clocks[1]) * self.fixed_clock
  self.frame_divider = 0
  self.frame_irq_clock = (band(data, 0xc0) ~= 0) and CPU.FOREVER_CLOCK or (n + APU.FRAME_CLOCKS[1])
  self.frame_irq_repeat = 0
  if nthBitIsSetInt(data, 6) ~= 0 then
    self.cpu:clear_irq(CPU.IRQ_FRAME)
  end
  if nthBitIsSetInt(data, 7) ~= 0 then
    self:clock_oscillators(true)
  end
end

function APU:peek_40xx(_addr)
  return 0x40
end

local LengthCounter = UTILS.class()
LengthCounter.LUT = {
  0x0a,
  0xfe,
  0x14,
  0x02,
  0x28,
  0x04,
  0x50,
  0x06,
  0xa0,
  0x08,
  0x3c,
  0x0a,
  0x0e,
  0x0c,
  0x1a,
  0x0e,
  0x0c,
  0x10,
  0x18,
  0x12,
  0x30,
  0x14,
  0x60,
  0x16,
  0xc0,
  0x18,
  0x48,
  0x1a,
  0x10,
  0x1c,
  0x20,
  0x1e
}
function LengthCounter:reset()
  self.enabled = false
  self.count = 0
end

function LengthCounter:enable(enabled)
  self.enabled = enabled
  if not self.enabled then
    self.count = 0
  end
  return self.enabled
end

function LengthCounter:write(data, frame_counter_delta)
  if frame_counter_delta or self.count == 0 then
    self.count = self.enabled and LengthCounter.LUT[data + 1] or 0
  end
end

function LengthCounter:clock()
  if self.count == 0 then
    return false
  end
  self.count = self.count - 1
  return self.count == 0
end

local Envelope = UTILS.class()

function Envelope:reset_clock()
  self.clock_reset = true
end

function Envelope:reset()
  self.output = 0
  self.count = 0
  self.volume_base = 0
  self.volume = 0
  self.constant = true
  self.looping = false
  self.clock_reset = false
  return self:update_output()
end

function Envelope:clock()
  if self.clock_reset then
    self.clock_reset = false
    self.volume = 0x0f
  else
    if self.count ~= 0 then
      self.count = self.count - 1
      return
    end
    if self.volume ~= 0 or self.looping then
      self.volume = band((self.volume - 1), 0x0f)
    end
  end
  self.count = self.volume_base
  return self:update_output()
end

function Envelope:write(data)
  self.volume_base = band(data, 0x0f)
  self.constant = nthBitIsSet(data, 4)
  self.looping = nthBitIsSet(data, 5)
  return self:update_output()
end

function Envelope:update_output()
  self.output = (self.constant and self.volume_base or self.volume)
end

MIXER = UTILS.class()
local MIXER = MIXER
MIXER.VOL = 192
MIXER.P_F = 900
MIXER.P_0 = 9552 * APU.CHANNEL_OUTPUT_MUL * MIXER.VOL * (MIXER.P_F / 100)
MIXER.P_1 = 8128 * APU.CHANNEL_OUTPUT_MUL * MIXER.P_F
MIXER.P_2 = MIXER.P_F * 100
MIXER.TND_F = 500
MIXER.TND_0 = 16367 * APU.CHANNEL_OUTPUT_MUL * MIXER.VOL * (MIXER.TND_F / 100)
MIXER.TND_1 = 24329 * APU.CHANNEL_OUTPUT_MUL * MIXER.TND_F
MIXER.TND_2 = MIXER.TND_F * 100

function MIXER:initialize(pulse_0, pulse_1, triangle, noise, dmc)
  self.pulse_0, self.pulse_1, self.triangle, self.noise, self.dmc = pulse_0, pulse_1, triangle, noise, dmc
end

function MIXER:reset()
  self.next = 0
  self.acc = 0
  self.prev = 0
end

function MIXER:sample()
  -- Formulas taken from https://wiki.nesdev.com/w/index.php/APU_Mixer
  --[[
    local dac0 = self.pulse_0:sample() + self.pulse_1:sample()
    local dac0 = 0.00752 * dac0
    local dac1 = 0 --0.00851 * self.triangle:sample() + 0.00494 * self.noise:sample() + 0.00335 * self.dmc:sample()
    --]]
  --[
  local dac0 = self.pulse_0:sample() + self.pulse_1:sample()
  local dac0 = 95.88 / ((8128 / dac0) + 100)
  -- TODO: DMC (For some reason it sounds really bad)
  local dac1 = 159.79 /
      (159 + 1 / ((self.triangle:sample() / 8227) + (self.noise:sample() / 12241))) --+ (self.dmc:sample() / 22638)))
  --]]
  return (dac0 + dac1)
  --[[
  local dac0 = self.pulse_0:sample() + self.pulse_1:sample()
  local dac1 = self.triangle:sample() + self.noise:sample() + self.dmc:sample()
  local sample =
    (MIXER.P_0 * dac0 / (MIXER.P_1 + MIXER.P_2 * dac0)) + (MIXER.TND_0 * dac1 / (MIXER.TND_1 + MIXER.TND_2 * dac1))

  self.acc = self.acc - self.prev
  self.prev = lshift(sample, 15)
  self.acc = self.acc + self.prev - self.next * 3 -- POLE
  self.next = rshift(self.acc, 15)
  sample = self.next

  if sample < -0x7fff then
    sample = -0x7fff
  end
  if sample > 0x7fff then
    sample = 0x7fff
  end
  return sample
  --]]
end

local Oscillator = UTILS.class()

function Oscillator:initialize(apu)
  self.apu = apu
  self.rate = 1
  self.fixed = 1
  self.envelope = nil
  self.length_counter = nil
  self.wave_length = nil
end

function Oscillator:reset()
  self.timer = 2048 * self.fixed -- 2048: reset cycles
  self.freq = self.fixed
  self.amp = 0

  if self.wave_length then
    self.wave_length = 0
  end
  if self.envelope then
    self.envelope:reset()
  end
  if self.length_counter then
    self.length_counter:reset()
  end
  self.is_active = self:active()
end

function Oscillator:active()
  if self.length_counter and self.length_counter.count == 0 then
    return false
  end
  if self.envelope and self.envelope.output == 0 then
    return false
  end
  return true
end

function Oscillator:poke_0(_addr, data)
  if self.envelope then
    self.apu:update_latency()
    self.envelope:write(data)
    self.is_active = self:active()
  end
end

function Oscillator:poke_2(_addr, data)
  self.apu:update()
  if self.wave_length then
    self.wave_length = bor(band(self.wave_length, 0x0700), band(data, 0x00ff))
    self:update_freq()
  end
end

function Oscillator:poke_3(_addr, data)
  local delta = self.apu:update_delta()
  if self.wave_length then
    self.wave_length = bor(band(self.wave_length, 0x00ff), lshift(band(data, 0x07), 8))
    self:update_freq()
  end
  if self.envelope then
    self.envelope:reset_clock()
  end
  if self.length_counter then
    self.length_counter:write(rshift(data, 3), delta)
  end
  self.is_active = self:active()
end

function Oscillator:enable(enabled)
  self.length_counter:enable(enabled)
  self.is_active = self:active()
end

function Oscillator:update_settings(r, f)
  self.freq = self.freq / self.fixed * f
  self.timer = self.timer / self.fixed * f
  self.rate, self.fixed = r, f
end

function Oscillator:status()
  return self.length_counter.count > 0
end

function Oscillator:clock_envelope()
  self.envelope:clock()
  self.is_active = self:active()
end

Pulse = UTILS.class(Oscillator)
local Pulse = Pulse
Pulse.MIN_FREQ = 0x0008
Pulse.MAX_FREQ = 0x07ff
--Pulse.WAVE_FORM = map{0b11111101, 0b11111001, 0b11100001, 0b00000110},function(n) return UTILS.map(range(0,7), function(i) return n[i] * 0x1f } end))
Pulse.WAVE_FORM = {
  { [0] = 0, 0, 0, 0, 0, 0, 0, 1 },
  { [0] = 0, 0, 0, 0, 0, 0, 1, 1 },
  { [0] = 0, 0, 0, 0, 1, 1, 1, 1 },
  { [0] = 1, 1, 1, 1, 1, 1, 0, 0 }
}

function Pulse:initialize(_apu)
  self._parent.initialize(self, _apu)
  self.wave_length = 0
  self.envelope = Envelope:new()
  self.length_counter = LengthCounter:new()
  self.duty = 0
end

function Pulse:reset()
  self._parent.reset(self)
  self.freq = self.fixed * 2
  self.period_timer = (2048 - (self.freq / 1000)) * 4
  self.valid_freq = false
  self.step = 0
  self.form = Pulse.WAVE_FORM[1]
  self.sweep_rate = 0
  self.sweep_count = 1
  self.sweep_reload = false
  self.sweep_increase = -1
  self.sweep_shift = 0
end

function Pulse:active()
  return self._parent.active(self) and self.valid_freq
end

function Pulse:update_freq()
  if
      self.wave_length >= Pulse.MIN_FREQ and
      self.wave_length + band(self.sweep_increase, rshift(self.wave_length, self.sweep_shift)) <= Pulse.MAX_FREQ
  then
    self.freq = (self.wave_length + 1) * 2 * self.fixed
    self.period_timer = (2048 - (self.freq / 1000)) * 4
    self.valid_freq = true
  else
    self.valid_freq = false
  end
  self.is_active = self:active()
end

function Pulse:poke_0(_addr, data)
  self._parent.poke_0(self, _addr, data)
  self.duty = band(rshift(data, 6), 3)
  self.form = Pulse.WAVE_FORM[1 + self.duty]
end

function Pulse:poke_1(_addr, data)
  self.apu:update()
  self.sweep_increase = nthBitIsSetInt(data, 3) ~= 0 and 0 or -1
  self.sweep_shift = band(data, 0x07)
  self.sweep_rate = 0
  if nthBitIsSetInt(data, 7) == 1 and self.sweep_shift > 0 then
    self.sweep_rate = band(rshift(data, 4), 0x07) + 1
    self.sweep_reload = true
  end
  return self:update_freq()
end

function Pulse:poke_3(_addr, data)
  self._parent.poke_3(self, _addr, data)
  self.step = 0
end

function Pulse:clock_sweep(complement)
  if not self.envelope.looping and self.length_counter:clock() then
    self.is_active = false
  end
  if self.sweep_rate ~= 0 then
    self.sweep_count = self.sweep_count - 1
    if self.sweep_count == 0 then
      self.sweep_count = self.sweep_rate
      if self.wave_length >= Pulse.MIN_FREQ then
        local shifted = rshift(self.wave_length, self.sweep_shift)
        if self.sweep_increase == 0 then
          self.wave_length = self.wave_length + complement - shifted
          self:update_freq()
        elseif self.wave_length + shifted <= Pulse.MAX_FREQ then
          self.wave_length = self.wave_length + shifted
          self:update_freq()
        end
      end
    end
  end

  if not self.sweep_reload then
    return
  end

  self.sweep_reload = false
  self.sweep_count = self.sweep_rate
end

function Pulse:sample()
  --[[
  if self.is_active then
    local rate = self.apu.settings_rate
    local samples_per_frame = rate / 60
    local cpu_clocks_per_frame = 29780
    -- rate/60 samples _ 29780 clocks
    -- 1 sample _ (29780*60)/rate clocks
    local period_timer_decrement_per_frame = cpu_clocks_per_frame / samples_per_frame
    self.period_timer = self.period_timer - period_timer_decrement_per_frame
    if self.period_timer <= 0 then
      self.period_timer = (2048 - (self.freq / 1000)) * 4
      self.step = band((self.step + 1), 7)
    end
    self.amp = self.envelope.output * self.form[self.step]
  else
    self.amp = 0
  end
  return self.amp
  --]]
  --[
  local sum = self.timer
  self.timer = self.timer - self.rate
  if self.is_active then
    if self.timer < 0 then
      sum = sum * self.form[self.step]
      repeat
        local v = -self.timer
        if v > self.freq then
          v = self.freq
        end
        self.step = band((self.step + 1), 7)
        sum = sum + v * self.form[self.step]
        self.timer = self.timer + self.freq
      until self.timer > 0
      self.amp = (sum * self.envelope.output) / self.rate
    else
      self.amp = self.envelope.output * self.form[self.step]
    end
  else
    if self.amp < APU.CHANNEL_OUTPUT_DECAY then
      return 0
    end
    self.amp = self.amp - APU.CHANNEL_OUTPUT_DECAY
  end
  return self.amp
  --]]
end

Triangle = UTILS.class(Oscillator)
local Triangle = Triangle
Triangle.MIN_FREQ = 2 + 1
Triangle.WAVE_FORM = concat0(range(0, 15), range(15, 0))

function Triangle:initialize(_apu)
  self._parent.initialize(self, _apu)
  self.wave_length = 0
  self.length_counter = LengthCounter:new()
end

function Triangle:reset()
  self._parent.reset(self)
  self.step = 7
  self.status = "counting"
  self.linear_counter_load = 0
  self.linear_counter_start = true
  self.linear_counter = 0
end

function Triangle:active()
  return self._parent.active(self) and self.linear_counter ~= 0 and self.wave_length >= Triangle.MIN_FREQ
end

function Triangle:update_freq()
  self.freq = (self.wave_length + 1) * self.fixed
  self.is_active = self:active()
end

function Triangle:poke_0(_addr, data)
  self._parent.poke_0(self, _addr, data)
  self.apu:update()
  self.linear_counter_load = band(data, 0x7f)
  self.linear_counter_start = nthBitIsSetInt(data, 7) == 0
end

function Triangle:poke_3(_addr, data)
  self._parent.poke_3(self, _addr, data)
  self.status = "reload"
end

function Triangle:clock_linear_counter()
  if self.status == "counting" then
    if self.linear_counter ~= 0 then
      self.linear_counter = self.linear_counter - 1
    end
  else
    if self.linear_counter_start then
      self.status = "counting"
    end
    self.linear_counter = self.linear_counter_load
  end
  self.is_active = self:active()
end

function Triangle:clock_length_counter()
  if self.linear_counter_start and self.length_counter:clock() then
    self.is_active = false
  end
end

function Triangle:sample()
  if self.is_active then
    local sum = self.timer
    self.timer = self.timer - self.rate
    if self.timer < 0 then
      sum = sum * Triangle.WAVE_FORM[self.step]
      repeat
        local v = math.min(-(self.timer), self.freq)
        self.step = band((self.step + 1), 0x1f)
        sum = sum + v * Triangle.WAVE_FORM[self.step]
        self.timer = self.timer + self.freq
      until not (self.timer < 0)
      self.amp = (sum + self.rate / 2) / self.rate
    else
      self.amp = Triangle.WAVE_FORM[self.step]
    end
  else
    if self.amp < APU.CHANNEL_OUTPUT_DECAY then
      return 0
    end
    self.amp = self.amp - APU.CHANNEL_OUTPUT_DECAY
    self.step = 0
  end
  return self.amp
end

Noise = UTILS.class(Oscillator)
local Noise = Noise
Noise.LUT = { 4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068 }
Noise.NEXT_BITS_1, Noise.NEXT_BITS_6 =
    unpack(
      UTILS.map(
        { 1, 6 },
        function(shifter)
          return UTILS.map(
            range(0, 0x7fff),
            function(bits)
              return math.floor(
                nthBitIsSetInt(bits, 0) == nthBitIsSetInt(bits, shifter) and (bits / 2) or (bits / 2 + 0x4000)
              )
            end
          )
        end
      )
    )

function Noise:initialize(_apu)
  self._parent.initialize(self, _apu)
  self.envelope = Envelope:new()
  self.length_counter = LengthCounter:new()
  self.bits = 0x4000
end

function Noise:reset()
  self._parent.reset(self)
  self.freq = Noise.LUT[1] * self.fixed
  self.bits = 0x4000
  self.shifter = Noise.NEXT_BITS_1
end

function Noise:poke_2(_addr, data)
  self.apu:update()
  self.freq = Noise.LUT[band(data, 0x0f) + 1] * self.fixed
  self.shifter = nthBitIsSetInt(data, 7) ~= 0 and Noise.NEXT_BITS_6 or Noise.NEXT_BITS_1
end

function Noise:clock_length_counter()
  if not self.envelope.looping and self.length_counter:clock() then
    self.is_active = false
  end
end

function Noise:sample()
  self.bits = self.bits or 0x4000
  self.timer = self.timer - self.rate
  if self.is_active then
    if self.timer >= 0 then
      return self.bits % 2 == 0 and self.envelope.output * 2 or 0
    end

    local sum = self.bits % 2 == 0 and (self.timer + self.rate) or 0
    repeat
      self.bits = self.shifter[self.bits]
      if self.bits % 2 == 0 then
        sum = sum + math.min(-self.timer, self.freq)
      end
      self.timer = self.timer + self.freq
    until not (self.timer < 0)
    return (sum * self.envelope.output + self.rate / 2) / self.rate * 2
  else
    while self.timer < 0 do
      self.bits = self.shifter[self.bits]
      self.timer = self.timer + self.freq
    end
    return 0
  end
end

DMC = UTILS.class()
local DMC = DMC
DMC.LUT =
    UTILS.map(
      { 428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54 },
      function(n)
        return n * CPU.RP2A03_CC
      end
    )

function DMC:initialize(cpu, apu)
  self.apu = apu
  self.cpu = cpu
  self.freq = DMC.LUT[1]
end

function DMC:reset()
  self.cur_sample = 0
  self.lin_sample = 0
  self.freq = DMC.LUT[1]
  self.loop = false
  self.irq_enable = false
  self.regs_length_counter = 1
  self.regs_address = 0xc000
  self.out_active = false
  self.out_shifter = 0
  self.out_dac = 0
  self.out_buffer = 0x00
  self.dma_length_counter = 0
  self.dma_buffered = false
  self.dma_address = 0xc000
  self.dma_buffer = 0x00
end

function DMC:enable(enabled)
  self.cpu:clear_irq(CPU.IRQ_DMC)
  if not enabled then
    self.dma_length_counter = 0
  elseif self.dma_length_counter == 0 then
    self.dma_length_counter = self.regs_length_counter
    self.dma_address = self.regs_address
    if not self.dma_buffered then
      self:do_dma()
    end
  end
end

function DMC:sample()
  if self.cur_sample ~= self.lin_sample then
    local step = APU.CHANNEL_OUTPUT_MUL * 8
    if self.lin_sample + step < self.cur_sample then
      self.lin_sample = self.lin_sample + step
    elseif self.cur_sample < self.lin_sample - step then
      self.lin_sample = self.lin_sample - step
    else
      self.lin_sample = self.cur_sample
    end
  end
  return self.lin_sample
end

function DMC:do_dma()
  self.dma_buffer = self.cpu:dmc_dma(self.dma_address)
  self.dma_address = bor(0x8000, band((self.dma_address + 1), 0x7fff))
  self.dma_buffered = true
  self.dma_length_counter = self.dma_length_counter - 1
  if self.dma_length_counter == 0 then
    if self.loop then
      self.dma_address = self.regs_address
      self.dma_length_counter = self.regs_length_counter
    elseif self.irq_enable then
      self.cpu:do_irq(CPU.IRQ_DMC, self.cpu:current_clock())
    end
  end
end

function DMC:update()
  self.cur_sample = self.out_dac * APU.CHANNEL_OUTPUT_MUL
end

function DMC:poke_0(_addr, data)
  self.loop = nthBitIsSetInt(data, 6) ~= 0
  self.irq_enable = nthBitIsSetInt(data, 7) ~= 0
  self.freq = DMC.LUT[band(data, 0x0f) + 1]
  if not self.irq_enable then
    self.cpu:clear_irq(CPU.IRQ_DMC)
  end
end

function DMC:poke_1(_addr, data)
  self.apu:update()
  self.out_dac = band(data, 0x7f)
  self:update()
end

function DMC:poke_2(_addr, data)
  self.regs_address = bor(0xc000, lshift(data, 6))
end

function DMC:poke_3(_addr, data)
  self.regs_length_counter = lshift(data, 4) + 1
end

function DMC:clock_dac()
  if self.out_active then
    n = self.out_dac + lshift(band(self.out_buffer, 1), 2) - 2
    self.out_buffer = rshift(self.out_buffer, 1)
    if 0 <= n and n <= 0x7f and n ~= self.out_dac then
      self.out_dac = n
      return true
    end
  end
  return false
end

function DMC:clock_dma()
  if self.out_shifter == 0 then
    self.out_shifter = 7
    self.out_active = self.dma_buffered
    if self.out_active then
      self.dma_buffered = false
      self.out_buffer = self.dma_buffer
      if self.dma_length_counter ~= 0 then
        self:do_dma()
      end
    end
  else
    self.out_shifter = self.out_shifter - 1
  end
end

function DMC:status()
  return self.dma_length_counter > 0
end
