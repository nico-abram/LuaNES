APU = {}
local APU = APU
APU._mt = {__index = APU}

local NES =
    Nes or
    {
        -- DUMMY NES
        RP2A03_CC = 12,
        FOREVER_CLOCK = 0xffffffff
    }
UTILS:import()
    APU.CLK_M2_MUL   = 6
    APU.CLK_NTSC     = 39375000 * APU.CLK_M2_MUL
    APU.CLK_NTSC_DIV = 11

    APU.CHANNEL_OUTPUT_MUL   = 256
    APU.CHANNEL_OUTPUT_DECAY = CHANNEL_OUTPUT_MUL / 4 - 1

    APU.FRAME_CLOCKS = map({29830, 1, 1, 29828}, function(n) return NES.RP2A03_CC * n end)
    APU.OSCILLATOR_CLOCKS = map({
      {7458, 7456, 7458, 7458},
      {7458, 7456, 7458, 7458 + 7452}
},function(a) map(function(n) return NES.RP2A03_CC * n end) end)
function APU:new(conf, cpu, palette)
    local apu = {}
    setmetatable(apu, APU._mt)
    apu:initialize(conf, apu, palette)
    return apu
end
    function APU:initialize(conf, cpu, rate, bits)
      self.conf = conf
      self.cpu = cpu

      self.pulse_0, self.pulse_1 = Pulse:new(self), Pulse:new(self)
      self.triangle = Triangle:new(self)
      self.noise = Noise:new(self)
      self.dmc = DMC:new(self.cpu, self)
      self.mixer = Mixer:new(self.pulse_0, self.pulse_1, self.triangle, self.noise, self.dmc)

      if rate < 11050 then self.conf.fatal("audio sample rate must be >= 11050")  end
      if bits ~= 8 and bits ~= 16 then self.conf.fatal("audio bit depth must be 8 or 16") end

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
    end

    function APU:reset_mapping()
      self.frame_counter = self.frame_counter  / self.fixed_clock
      self.rate_counter = self.rate_counter / self.fixed_clock
      local multiplier = 0
      while true do
        multiplier =multiplier + 1
        break if multiplier >= 512
        break if CLK_NTSC * multiplier % self.settings_rate == 0
      end
      self.rate_clock = CLK_NTSC * multiplier / self.settings_rate
      self.fixed_clock = CLK_NTSC_DIV * multiplier
      self.frame_counter =self.frame_counter * self.fixed_clock
      self.rate_counter =self.rate_counter * self.fixed_clock

      self.mixer.reset
      self.buffer.clear

      multiplier = 0
      while true
        multiplier =multiplier + 1
        break if multiplier >= 0x1000
        break if CLK_NTSC * (multiplier + 1) / self.settings_rate > 0x7ffff
        break if CLK_NTSC * multiplier % self.settings_rate == 0
      end
      rate = CLK_NTSC * multiplier / self.settings_rate
      fixed = CLK_NTSC_DIV * CPU::CLK_1 * multiplier

      self.pulse_0:update_settings(rate, fixed)
      self.pulse_1:update_settings(rate, fixed)
      self.triangle:update_settings(rate, fixed)
      self.noise:update_settings(rate, fixed)

      self.cpu:add_mappings(0x4000, bind(self.peek_40xx, self), bind(self.pulse_0 .poke_0, self.pulse_0 ))
      self.cpu:add_mappings(0x4001, bind(self.peek_40xx, self), bind(self.pulse_0 .poke_1, self.pulse_0 ))
      self.cpu:add_mappings(0x4002, bind(self.peek_40xx, self), bind(self.pulse_0 .poke_2, self.pulse_0 ))
      self.cpu:add_mappings(0x4003, bind(self.peek_40xx, self), bind(self.pulse_0 .poke_3, self.pulse_0 ))
      self.cpu:add_mappings(0x4004, bind(self.peek_40xx, self), bind(self.pulse_1 .poke_0, self.pulse_1 ))
      self.cpu:add_mappings(0x4005, bind(self.peek_40xx, self), bind(self.pulse_1 .poke_1, self.pulse_1 ))
      self.cpu:add_mappings(0x4006, bind(self.peek_40xx, self), bind(self.pulse_1 .poke_2, self.pulse_1 ))
      self.cpu:add_mappings(0x4007, bind(self.peek_40xx, self), bind(self.pulse_1 .poke_3, self.pulse_1 ))
      self.cpu:add_mappings(0x4008, bind(self.peek_40xx, self), bind(self.triangle.poke_0, self.triangle))
      self.cpu:add_mappings(0x400a, bind(self.peek_40xx, self), bind(self.triangle.poke_2, self.triangle))
      self.cpu:add_mappings(0x400b, bind(self.peek_40xx, self), bind(self.triangle.poke_3, self.triangle))
      self.cpu:add_mappings(0x400c, bind(self.peek_40xx, self), bind(self.noise   .poke_0, self.noise   ))
      self.cpu:add_mappings(0x400e, bind(self.peek_40xx, self), bind(self.noise   .poke_2, self.noise   ))
      self.cpu:add_mappings(0x400f, bind(self.peek_40xx, self), bind(self.noise   .poke_3, self.noise   ))
      self.cpu:add_mappings(0x4010, bind(self.peek_40xx, self), bind(self.dmc     .poke_0, self.dmc     ))
      self.cpu:add_mappings(0x4011, bind(self.peek_40xx, self), bind(self.dmc     .poke_1, self.dmc     ))
      self.cpu:add_mappings(0x4012, bind(self.peek_40xx, self), bind(self.dmc     .poke_2, self.dmc     ))
      self.cpu:add_mappings(0x4013, bind(self.peek_40xx, self), bind(self.dmc     .poke_3, self.dmc     ))
      self.cpu:add_mappings(0x4015, bind(self.peek_4015, self), bind(self.poke_4015, self))
      self.frame_irq_clock = (self.frame_counter / self.fixed_clock) - CPU.CLK[1]
    end

    function APU:reset(mapping )
        mapping = mapping or true
      self.cycles_ratecounter = 0
      self.frame_divider = 0
      self.frame_irq_clock = FOREVER_CLOCK
      self.frame_irq_repeat = 0
      self.dmc_clock = DMC::LUT[0]
      self.frame_counter = FRAME_CLOCKS[0] * self.fixed_clock

      if mapping then reset_mapping end

      self.pulse_0:reset()
      self.pulse_1:reset()
      self.triangle:reset()
      self.noise:reset()
      self.dmc:reset()
      self.mixer:reset()
      self.buffer:clear()
      self.oscillator_clocks = OSCILLATOR_CLOCKS[0]
    end

    ###########################################################################
    # other APIs


    function APU:do_clock()
      self:clock_dma(self.cpu.current_clock)
      self:clock_frame_irq(self.cpu.current_clock) if self.frame_irq_clock <= self.cpu.current_clock
      return self.dmc_clock < self.frame_irq_clock and self.dmc_clock or self.frame_irq_clock
    end

    function APU:clock_dma(clk)
       if self.dmc_clock <= clk then clock_dmc(clk) end
    end

    function APU:update(target = self.cpu.update)
      target =target * self.fixed_clock
      self:proceed(target)
      if self.frame_counter < target then clock_frame_counter  end
    end

    function APU:update_latency()
      elf:update(self.cpu:update() + 1)
    end

    function APU:update_delta()
      local elapsed = self.cpu:update()
      --TODO
      local delta = self.frame_counter ~= elapsed * self.fixed_clock
      self:update(elapsed + 1)
      return delta
    end

    function APU:vsync()
      self:flush_sound()
      self:update(self.cpu.current_clock)
      local frame = self.cpu.next_frame_clock
      self.dmc_clock =self.dmc_clock - frame
      if self.frame_irq_clock ~= FOREVER_CLOCK then
      self.frame_irq_clock =self.frame_irq_clock - frame  
    end
      frame =frame * self.fixed_clock
      self.rate_counter =self.rate_counter - frame
      self.frame_counter =self.frame_counter - frame
    end

    ###########################################################################
    # helpers

    function APU:clock_oscillators(two_clocks)
      self.pulse_0:clock_envelope()
      self.pulse_1:clock_envelope()
      self.triangle:clock_linear_counter()
      self.noise:clock_envelope()
      if not two_clocks then return end
      self.pulse_0:clock_sweep(-1)
      self.pulse_1:clock_sweep(0)
      self.triangle:clock_length_counter()
      self.noise:clock_length_counter()
    end

    function APU:clock_dmc(target)
      do
        if self.dmc.clock_dac then
          self:update(self.dmc_clock)
          self.dmc.update
        end
        self.dmc_clock =self.dmc_clock + self.dmc.freq
        self.dmc.clock_dma
        -- TODO: IS THIS RIGHT?
      repeat until not self.dmc_clock <= target
    end

    function APU:clock_frame_counter()
      self:clock_oscillators(self.frame_divider[0] == 1)
      self.frame_divider = bit.band(self.frame_divider + 1) , 3)
      self.frame_counter =self.frame_counter + self.oscillator_clocks[self.frame_divider] * self.fixed_clock
    end

    function APU:clock_frame_irq(target)
      self.cpu:do_irq(CPU.IRQ_FRAME, self.frame_irq_clock)
      do
        self.frame_irq_clock =self.frame_irq_clock + FRAME_CLOCKS[1 + self.frame_irq_repeat % 3]
        self.frame_irq_repeat =self.frame_irq_repeat + 1
      repeat until not self.frame_irq_clock <= target
    end

    function APU:flush_sound()
      if self.buffer.size < self.settings_rate / 60 then
        target = self.cpu.current_clock * self.fixed_clock
        proceed(target)
        if self.buffer.size < self.settings_rate / 60 then
          clock_frame_counter if self.frame_counter < target
          self.buffer << self.mixer.sample while self.buffer.size < self.settings_rate / 60
        end
      end
      self.output = {} --.clear
      self.output = concat(self.output,self.buffer)-- Array#replace creates an object internally
      self.buffer = {} --.clear
    end

    function APU:proceed(target)
      while self.rate_counter < target and self.buffer.size < self.settings_rate / 60
        self.buffer[#(self.buffer)+1] =  self.mixer.sample
        if self.frame_counter <= self.rate_counter then self:clock_frame_counter() end
        self.rate_counter =self.rate_counter + self.rate_clock
      end
    end

    ###########################################################################
    # mapped memory handlers

    # Control
    function APU:poke_4015(_addr, data)
      self:update()
      self.pulse_0:enable(data[0] == 1)
      self.pulse_1:enable(data[1] == 1)
      self.triangle:enable(data[2] == 1)
      self.noise:enable(data[3] == 1)
      self.dmc:enable(data[4] == 1)
    end

    # Status
    function APU:peek_4015(_addr)
      elapsed = self.cpu.update
      clock_frame_irq(elapsed) if self.frame_irq_clock <= elapsed
      update(elapsed) if self.frame_counter < elapsed * self.fixed_clock
      self.cpu.clear_irq(CPU::IRQ_FRAME) |
        (self.pulse_0 .status ? 0x01 : 0) |
        (self.pulse_1 .status ? 0x02 : 0) |
        (self.triangle.status ? 0x04 : 0) |
        (self.noise   .status ? 0x08 : 0) |
        (self.dmc     .status ? 0x10 : 0)
    end

    # Frame counter (NOTE: this handler is called via Pads)
    function APU:poke_4017(_addr, data)
      local n = self.cpu:update()
      if self.cpu:odd_clock() thens n =n+ CPU.CLK[1] end
      update(n)
      clock_frame_irq(n) if self.frame_irq_clock <= n
      n = n+ CPU.CLK[1]
      self.oscillator_clocks = OSCILLATOR_CLOCKS[data[7]]
      self.frame_counter = (n + self.oscillator_clocks[0]) * self.fixed_clock
      self.frame_divider = 0
      self.frame_irq_clock = (bit.band(data , 0xc0) ~= 0) and FOREVER_CLOCK or (n + FRAME_CLOCKS[0])
      self.frame_irq_repeat = 0
       if data[6] ~= 0 then self.cpu.clear_irq(CPU::IRQ_FRAME) end
      if data[7] ~= 0 then self:clock_oscillators(true)  end
    end

    function APU:peek_40xx(_addr)
    	return 0x40
    end
--[[
    ###########################################################################
    # helper classes

    # A counter for note length
    class LengthCounter
      LUT = [
        0x0a, 0xfe, 0x14, 0x02, 0x28, 0x04, 0x50, 0x06, 0xa0, 0x08, 0x3c, 0x0a, 0x0e, 0x0c, 0x1a, 0x0e,
        0x0c, 0x10, 0x18, 0x12, 0x30, 0x14, 0x60, 0x16, 0xc0, 0x18, 0x48, 0x1a, 0x10, 0x1c, 0x20, 0x1e,
      ]
      def reset
        self.enabled = false
        self.count = 0
      end

      attr_reader :count

      def enable(enabled)
        self.enabled = enabled
        self.count = 0 unless self.enabled
        self.enabled
      end

      def write(data, frame_counter_delta)
        self.count = self.enabled ? LUT[data] : 0 if frame_counter_delta or self.count == 0
      end

      def clock
        return false if self.count == 0
        self.count -= 1
        return self.count == 0
      end
    end

    # Wave envelope
    class Envelope
      attr_reader :output, :looping

      def reset_clock
        self.reset = true
      end

      def reset
        self.output = 0
        self.count = 0
        self.volume_base = self.volume = 0
        self.constant = true
        self.looping = false
        self.reset = false
        update_output
      end

      def clock
        if self.reset
          self.reset = false
          self.volume = 0x0f
        else
          if self.count ~= 0
            self.count -= 1
            return
          end
          self.volume = (self.volume - 1) & 0x0f if self.volume ~= 0 or self.looping
        end
        self.count = self.volume_base
        update_output
      end

      def write(data)
        self.volume_base = data & 0x0f
        self.constant = data[4] == 1
        self.looping = data[5] == 1
        update_output
      end

      def update_output
        self.output = (self.constant ? self.volume_base : self.volume) * CHANNEL_OUTPUT_MUL
      end
    end

    # Mixer (with DC Blocking filter)
    class Mixer
      VOL   = 192
      P_F   = 900
      P_0   = 9552 * CHANNEL_OUTPUT_MUL * VOL * (P_F / 100)
      P_1   = 8128 * CHANNEL_OUTPUT_MUL * P_F
      P_2   = P_F * 100
      TND_F = 500
      TND_0 = 16367 * CHANNEL_OUTPUT_MUL * VOL * (TND_F / 100)
      TND_1 = 24329 * CHANNEL_OUTPUT_MUL * TND_F
      TND_2 = TND_F * 100

      def initialize(pulse_0, pulse_1, triangle, noise, dmc)
        self.pulse_0, self.pulse_1, self.triangle, self.noise, self.dmc = pulse_0, pulse_1, triangle, noise, dmc
      end

      def reset
        self.acc = self.prev = self.next = 0
      end

      def sample
        dac0 = self.pulse_0.sample + self.pulse_1.sample
        dac1 = self.triangle.sample + self.noise.sample + self.dmc.sample
        sample = (P_0 * dac0 / (P_1 + P_2 * dac0)) + (TND_0 * dac1 / (TND_1 + TND_2 * dac1))

        self.acc -= self.prev
        self.prev = sample << 15
        self.acc += self.prev - self.next * 3 # POLE
        sample = self.next = self.acc >> 15

        sample = -0x7fff if sample < -0x7fff
        sample = 0x7fff if sample > 0x7fff
        sample
      end
    end

    # base class for oscillator channels (Pulse, Triangle, and Noise)
    class Oscillator
      def inspect
        "#<#{ self.class }>"
      end

      def initialize(apu)
        self.apu = apu
        self.rate = self.fixed = 1
        self.envelope = self.length_counter = self.wave_length = nil
      end

      def reset
        self.timer = 2048 * self.fixed # 2048: reset cycles
        self.freq = self.fixed
        self.amp = 0

        self.wave_length = 0 if self.wave_length
        self.envelope.reset if self.envelope
        self.length_counter.reset if self.length_counter
        self.active = active?
      end

      def active?
        return false if self.length_counter and self.length_counter.count == 0
        return false if self.envelope and self.envelope.output == 0
        return true
      end

      def poke_0(_addr, data)
        if self.envelope
          self.apu.update_latency
          self.envelope.write(data)
          self.active = active?
        end
      end

      def poke_2(_addr, data)
        self.apu.update
        if self.wave_length
          self.wave_length = (self.wave_length & 0x0700) | (data & 0x00ff)
          update_freq
        end
      end

      def poke_3(_addr, data)
        delta = self.apu.update_delta
        if self.wave_length
          self.wave_length = (self.wave_length & 0x00ff) | ((data & 0x07) << 8)
          update_freq
        end
        self.envelope.reset_clock if self.envelope
        self.length_counter.write(data >> 3, delta) if self.length_counter
        self.active = active?
      end

      def enable(enabled)
        self.length_counter.enable(enabled)
        self.active = active?
      end

      def update_settings(r, f)
        self.freq = self.freq / self.fixed * f
        self.timer = self.timer / self.fixed * f
        self.rate, self.fixed = r, f
      end

      def status
        self.length_counter.count > 0
      end

      def clock_envelope
        self.envelope.clock
        self.active = active?
      end
    end

    #--------------------------------------------------------------------------

    ### Pulse channel ###
    class Pulse < Oscillator
      MIN_FREQ = 0x0008
      MAX_FREQ = 0x07ff
      WAVE_FORM = [0b11111101, 0b11111001, 0b11100001, 0b00000110].map {|n| (0..7).map {|i| n[i] * 0x1f } }

      def initialize(_apu)
        super
        self.wave_length = 0
        self.envelope = Envelope.new
        self.length_counter = LengthCounter.new
      end

      def reset
        super
        self.freq = self.fixed * 2
        self.valid_freq = false
        self.step = 0
        self.form = WAVE_FORM[0]
        self.sweep_rate = 0
        self.sweep_count = 1
        self.sweep_reload = false
        self.sweep_increase = -1
        self.sweep_shift = 0
      end

      def active?
        super and self.valid_freq
      end

      def update_freq
        if self.wave_length >= MIN_FREQ and self.wave_length + (self.sweep_increase & self.wave_length >> self.sweep_shift) <= MAX_FREQ
          self.freq = (self.wave_length + 1) * 2 * self.fixed
          self.valid_freq = true
        else
          self.valid_freq = false
        end
        self.active = active?
      end

      def poke_0(_addr, data)
        super
        self.form = WAVE_FORM[data >> 6 & 3]
      end

      def poke_1(_addr, data)
        self.apu.update
        self.sweep_increase = data[3] ~= 0 ? 0 : -1
        self.sweep_shift = data & 0x07
        self.sweep_rate = 0
        if data[7] == 1 and self.sweep_shift > 0
          self.sweep_rate = ((data >> 4) & 0x07) + 1
          self.sweep_reload = true
        end
        update_freq
      end

      def poke_3(_addr, _data)
        super
        self.step = 0
      end

      def clock_sweep(complement)
        self.active = false if !self.envelope.looping and self.length_counter.clock
        if self.sweep_rate ~= 0
          self.sweep_count -= 1
          if self.sweep_count == 0
            self.sweep_count = self.sweep_rate
            if self.wave_length >= MIN_FREQ
              shifted = self.wave_length >> self.sweep_shift
              if self.sweep_increase == 0
                self.wave_length += complement - shifted
                update_freq
              elsif self.wave_length + shifted <= MAX_FREQ
                self.wave_length += shifted
                update_freq
              end
            end
          end
        end

        return unless self.sweep_reload

        self.sweep_reload = false
        self.sweep_count = self.sweep_rate
      end

      def sample
        sum = self.timer
        self.timer -= self.rate
        if self.active
          if self.timer < 0
            sum >>= self.form[self.step]
            begin
              v = -self.timer
              v = self.freq if v > self.freq
              sum += v >> self.form[self.step = (self.step + 1) & 7]
              self.timer += self.freq
            end while self.timer < 0
            self.amp = (sum * self.envelope.output + self.rate / 2) / self.rate
          else
            self.amp = self.envelope.output >> self.form[self.step]
          end
        else
          if self.timer < 0
            count = (-self.timer + self.freq - 1) / self.freq
            self.step = (self.step + count) & 7
            self.timer += count * self.freq
          end
          return 0 if self.amp < CHANNEL_OUTPUT_DECAY
          self.amp -= CHANNEL_OUTPUT_DECAY
        end
        self.amp
      end
    end

    #--------------------------------------------------------------------------

    ### Triangle channel ###
    class Triangle < Oscillator
      MIN_FREQ = 2 + 1
      WAVE_FORM = (0..15).to_a + (0..15).to_a.reverse

      def initialize(_apu)
        super
        self.wave_length = 0
        self.length_counter = LengthCounter.new
      end

      def reset
        super
        self.step = 7
        self.status = :counting
        self.linear_counter_load = 0
        self.linear_counter_start = true
        self.linear_counter = 0
      end

      def active?
        super and self.linear_counter ~= 0 and self.wave_length >= MIN_FREQ
      end

      def update_freq
        self.freq = (self.wave_length + 1) * self.fixed
        self.active = active?
      end

      def poke_0(_addr, data)
        super
        self.apu.update
        self.linear_counter_load = data & 0x7f
        self.linear_counter_start = data[7] == 0
      end

      def poke_3(_addr, _data)
        super
        self.status = :reload
      end

      def clock_linear_counter
        if self.status == :counting
          self.linear_counter -= 1 if self.linear_counter ~= 0
        else
          self.status = :counting if self.linear_counter_start
          self.linear_counter = self.linear_counter_load
        end
        self.active = active?
      end

      def clock_length_counter
        self.active = false if self.linear_counter_start and self.length_counter.clock
      end

      def sample
        if self.active
          sum = self.timer
          self.timer -= self.rate
          if self.timer < 0
            sum *= WAVE_FORM[self.step]
            begin
              v = -self.timer
              v = self.freq if v > self.freq
              sum += v * WAVE_FORM[self.step = (self.step + 1) & 0x1f]
              self.timer += self.freq
            end while self.timer < 0
            self.amp = (sum * CHANNEL_OUTPUT_MUL + self.rate / 2) / self.rate * 3
          else
            self.amp = WAVE_FORM[self.step] * CHANNEL_OUTPUT_MUL * 3
          end
        else
          return 0 if self.amp < CHANNEL_OUTPUT_DECAY
          self.amp -= CHANNEL_OUTPUT_DECAY
          self.step = 0
        end
        self.amp
      end
    end

    #--------------------------------------------------------------------------

    ### Noise channel ###
    class Noise < Oscillator
      LUT = [4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068]
      NEXT_BITS_1, NEXT_BITS_6 = [1, 6].map do |shifter|
        (0..0x7fff).map {|bits| bits[0] == bits[shifter] ? bits / 2 : bits / 2 + 0x4000 }
      end

      def initialize(_apu)
        super
        self.envelope = Envelope.new
        self.length_counter = LengthCounter.new
      end

      def reset
        super
        self.freq = LUT[0] * self.fixed
        self.bits = 0x4000
        self.shifter = NEXT_BITS_1
      end

      def poke_2(_addr, data)
        self.apu.update
        self.freq = LUT[data & 0x0f] * self.fixed
        self.shifter = data[7] ~= 0 ? NEXT_BITS_6 : NEXT_BITS_1
      end

      def clock_length_counter
        self.active = false if !self.envelope.looping and self.length_counter.clock
      end

      def sample
        self.timer -= self.rate
        if self.active
          return self.bits.even? ? self.envelope.output * 2 : 0 if self.timer >= 0

          sum = self.bits.even? ? self.timer : 0
          begin
            self.bits = self.shifter[self.bits]
            if self.bits.even?
              v = -self.timer
              v = self.freq if v > self.freq
              sum += v
            end
            self.timer += self.freq
          end while self.timer < 0
          return (sum * self.envelope.output + self.rate / 2) / self.rate * 2
        else
          while self.timer < 0
            self.bits = self.shifter[self.bits]
            self.timer += self.freq
          end
          return 0
        end
      end
    end

    #--------------------------------------------------------------------------

    ### DMC channel ###
    class DMC
      LUT = [428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54].map {|n| n * NES.RP2A03_CC }

      def initialize(cpu, apu)
        self.apu = apu
        self.cpu = cpu
        self.freq = LUT[0]
      end

      def reset
        self.cur_sample          = 0
        self.lin_sample          = 0
        self.freq                = LUT[0]
        self.loop                = false
        self.irq_enable          = false
        self.regs_length_counter = 1
        self.regs_address        = 0xc000
        self.out_active          = false
        self.out_shifter         = 0
        self.out_dac             = 0
        self.out_buffer          = 0x00
        self.dma_length_counter  = 0
        self.dma_buffered        = false
        self.dma_address         = 0xc000
        self.dma_buffer          = 0x00
      end

      attr_reader :freq

      def enable(enabled)
        self.cpu.clear_irq(CPU::IRQ_DMC)
        if !enabled
          self.dma_length_counter = 0
        elsif self.dma_length_counter == 0
          self.dma_length_counter = self.regs_length_counter
          self.dma_address = self.regs_address
          do_dma unless self.dma_buffered
        end
      end

      def sample
        if self.cur_sample ~= self.lin_sample
          step = CHANNEL_OUTPUT_MUL * 8
          if self.lin_sample + step < self.cur_sample
            self.lin_sample += step
          elsif self.cur_sample < self.lin_sample - step
            self.lin_sample -= step
          else
            self.lin_sample = self.cur_sample
          end
        end
        self.lin_sample
      end

      def do_dma
        self.dma_buffer = self.cpu.dmc_dma(self.dma_address)
        self.dma_address = 0x8000 | ((self.dma_address + 1) & 0x7fff)
        self.dma_buffered = true
        self.dma_length_counter -= 1
        if self.dma_length_counter == 0
          if self.loop
            self.dma_address = self.regs_address
            self.dma_length_counter = self.regs_length_counter
          elsif self.irq_enable
            self.cpu.do_irq(CPU::IRQ_DMC, self.cpu.current_clock)
          end
        end
      end

      def update
        self.cur_sample = self.out_dac * CHANNEL_OUTPUT_MUL
      end

      def poke_0(_addr, data)
        self.loop = data[6] ~= 0
        self.irq_enable = data[7] ~= 0
        self.freq = LUT[data & 0x0f]
        self.cpu.clear_irq(CPU::IRQ_DMC) unless self.irq_enable
      end

      def poke_1(_addr, data)
        self.apu.update
        self.out_dac = data & 0x7f
        update
      end

      def poke_2(_addr, data)
        self.regs_address = 0xc000 | (data << 6)
      end

      def poke_3(_addr, data)
        self.regs_length_counter = (data << 4) + 1
      end

      def clock_dac
        if self.out_active
          n = self.out_dac + ((self.out_buffer & 1) << 2) - 2
          self.out_buffer >>= 1
          if 0 <= n and n <= 0x7f and n ~= self.out_dac
            self.out_dac = n
            return true
          end
        end
        return false
      end

      def clock_dma
        if self.out_shifter == 0
          self.out_shifter = 7
          self.out_active = self.dma_buffered
          if self.out_active
            self.dma_buffered = false
            self.out_buffer = self.dma_buffer
            do_dma if self.dma_length_counter ~= 0
          end
        else
          self.out_shifter -= 1
        end
      end

      def status
        self.dma_length_counter > 0
      end
    end
  end
end
      ]]