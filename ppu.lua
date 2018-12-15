
PPU = {}
local PPU = PPU
PPU._mt = {__index = PPU}
    -- clock/timing constants (stolen from Nestopia)
    PPU.RP2C02_CC         = 4
    PPU.RP2C02_HACTIVE    = PPU.RP2C02_CC * 256
    PPU.RP2C02_HBLANK     = PPU.RP2C02_CC * 85
    PPU.RP2C02_HSYNC      = PPU.RP2C02_HACTIVE + PPU.RP2C02_HBLANK
    PPU.RP2C02_VACTIVE    = 240
    PPU.RP2C02_VSLEEP     = 1
    PPU.RP2C02_VINT       = 20
    PPU.RP2C02_VDUMMY     = 1
    PPU.RP2C02_VBLANK     = PPU.RP2C02_VSLEEP + PPU.RP2C02_VINT + PPU.RP2C02_VDUMMY
    PPU.RP2C02_VSYNC      = PPU.RP2C02_VACTIVE + PPU.RP2C02_VBLANK
    PPU.RP2C02_HVSYNCBOOT = PPU.RP2C02_VACTIVE * PPU.RP2C02_HSYNC + PPU.RP2C02_CC * 312
    PPU.RP2C02_HVINT      = PPU.RP2C02_VINT * PPU.RP2C02_HSYNC
    PPU.RP2C02_HVSYNC_0   = PPU.RP2C02_VSYNC * PPU.RP2C02_HSYNC
    PPU.RP2C02_HVSYNC_1   = PPU.RP2C02_VSYNC * PPU.RP2C02_HSYNC - PPU.RP2C02_CC

    -- special scanlines
    PPU.SCANLINE_HDUMMY = -1  -- pre-render scanline
    PPU.SCANLINE_VBLANK = 240 -- post-render scanline

    -- special horizontal clocks
    PPU.HCLOCK_DUMMY    = 341
    PPU.HCLOCK_VBLANK_0 = 681
    PPU.HCLOCK_VBLANK_1 = 682
    PPU.HCLOCK_VBLANK_2 = 684
    PPU.HCLOCK_BOOT     = 685
    PPU.DUMMY_FRAME = {PPU.RP2C02_HVINT / PPU.RP2C02_CC - PPU.HCLOCK_DUMMY, PPU.RP2C02_HVINT, PPU.RP2C02_HVSYNC_0}
    PPU.BOOT_FRAME = {PPU.RP2C02_HVSYNCBOOT / PPU.RP2C02_CC - PPU.HCLOCK_BOOT, PPU.RP2C02_HVSYNCBOOT, PPU.RP2C02_HVSYNCBOOT}

    -- constants related to OAM (sprite)
    PPU.SP_PIXEL_POSITIONS = {
       {3, 7, 2, 6, 1, 5, 0, 4}, -- normal
       {4, 0, 5, 1, 6, 2, 7, 3}, -- flip
    }

    -- A look-up table mapping: (two pattern bytes * attr) -> eight pixels
    --   TILE_LUT[attr][high_byte * 0x100 + low_byte] = [pixels] * 8
    TILE_LUT = map({0x0, 0x4, 0x8, 0xc}, function (attr)
        map(range(0,7), function(j) 
            return transpose(map(range(0,0x10000), function(i) 
                local clr = nthBitIsSetInt(i,15 - j) * 2 + nthBitIsSetInt(i,7 - j)
                return clr ~= 0 and bit.bor( attr, clr) or 0
            end))
        end)
        --[[
      (0..7).map do |j|
        (0...0x10000).map do |i|
          clr = i[15 - j] * 2 + i[7 - j]
          clr ~= 0 ? attr | clr : 0
        end
      end.transpose
      ]]
      -- Super dirty hack: This Array--transpose reduces page-faults.
      -- It might generate cache-friendly memory layout...
    end)

    ------------------------------------------------------------------------------------------------------------------------------------------------------
    -- initialization

    function PPU:initialize(conf, cpu, palette)
      self.conf = conf
      self.cpu = cpu
      self.palette = palette

      self.nmt_mem = {fill({}, 0xff,0x400),fill({}, 0xff,0x400)}--[  [0xff] * 0x400, [0xff] * 0x400]
      self.nmt_ref = map({0, 1, 0, 1},function(i) return self.nmt_mem[i] end)

      self.output_pixels = {}
      self.output_color = fill({},{self.palette[0]}, 0x20) -- palette size is 0x20

      reset(false)
      self:setup_lut()
    end

    function PPU:reset(mapping)
      if mapping then
        -- setup mapped memory
        self.cpu.add_mappings(range(0x2000,0x3fff, 8), bind(self.peek_2xxx, self), bind(self.poke_2000, self))
        self.cpu.add_mappings(range(0x2001,0x3fff, 8), bind(self.peek_2xxx, self), bind(self.poke_2001, self))
        self.cpu.add_mappings(range(0x2002,0x3fff, 8), bind(self.peek_2002, self), bind(self.poke_2xxx, self))
        self.cpu.add_mappings(range(0x2003,0x3fff, 8), bind(self.peek_2xxx, self), bind(self.poke_2003, self))
        self.cpu.add_mappings(range(0x2004,0x3fff, 8), bind(self.peek_2004, self), bind(self.poke_2004, self))
        self.cpu.add_mappings(range(0x2005,0x3fff, 8), bind(self.peek_2xxx, self), bind(self.poke_2005, self))
        self.cpu.add_mappings(range(0x2006,0x3fff, 8), bind(self.peek_2xxx, self), bind(self.poke_2006, self))
        self.cpu.add_mappings(range(0x2007,0x3fff, 8), bind(self.peek_2007, self), bind(self.poke_2007, self))
        self.cpu.add_mappings(0x3000, bind(self.peek_3000, self), bind(self.poke_2000, self))
        self.cpu.add_mappings(0x4014, bind(self.peek_4014, self), bind(self.poke_4014, self))
      end

      self.palette_ram = {
        0x3f, 0x01, 0x00, 0x01, 0x00, 0x02, 0x02, 0x0d,
        0x08, 0x10, 0x08, 0x24, 0x00, 0x00, 0x04, 0x2c,
        0x09, 0x01, 0x34, 0x03, 0x00, 0x04, 0x00, 0x14,
        0x08, 0x3a, 0x00, 0x02, 0x00, 0x20, 0x2c, 0x08,
    }
      self.coloring = 0x3f -- not monochrome
      self.emphasis = 0
      self:update_output_color()

      -- clock management
      self.hclk = PPU.HCLOCK_BOOT
      self.vclk = 0
      self.hclk_target = CPU.FOREVER_CLOCK

      -- CPU-PPU interface
      self.io_latch = 0
      self.io_buffer = 0xe8 -- garbage

      self.regs_oam = 0

      -- misc
      self.vram_addr_inc = 1 -- 1 or 32
      self.need_nmi = false
      self.pattern_end = 0x0ff0
      self.any_show = false -- == self.bg_show or self.sp_show
      self.sp_overflow = false
      self.sp_zero_hit = false
      self.vblanking = false
      self.vblank = false

      -- PPU-nametable interface
      self.io_addr = 0
      self.io_pattern = 0

      self.a12_monitor = nil
      self.a12_state = nil

      -- the current scanline
      self.odd_frame = false
      self.scanline = PPU.SCANLINE_VBLANK

      -- scroll state
      self.scroll_toggle = false
      self.scroll_latch = 0
      self.scroll_xfine = 0
      self.scroll_addr_0_4 = 0
      self.scroll_addr_5_14 = 0
      self.name_io_addr = 0x2000 -- == (self.scroll_addr_0_4 | self.scroll_addr_5_14) & 0x0fff | 0x2000

      ------ BG-sprite state
      self.bg_enabled = false
      self.bg_show = false
      self.bg_show_edge = false
      self.bg_pixels = fill({},{0}, 16)
      self.bg_pattern_base = 0 -- == 0 or 0x1000
      self.bg_pattern_base_15 = 0 -- == self.bg_pattern_base[12] << 15
      self.bg_pattern = 0
      self.bg_pattern_lut = PPU.TILE_LUT[1]
      self.bg_pattern_lut_fetched = PPU.TILE_LUT[1]
      -- invariant:
      --   self.bg_pattern_lut_fetched == TILE_LUT[
      --     self.nmt_ref[self.io_addr >> 10 & 3][self.io_addr & 0x03ff] >>
      --       ((self.scroll_addr_0_4 & 0x2) | (self.scroll_addr_5_14[6] * 0x4)) & 3
      --   ]

      ------ OAM-sprite state
      self.sp_enabled = false
      self.sp_active = false -- == self.sp_visible and self.sp_enabled
      self.sp_show = false
      self.sp_show_edge = false

      -- for CPU-PPU interface
      self.sp_base = 0
      self.sp_height = 8

      -- for OAM fetcher
      self.sp_phase = 0
      self.sp_ram = fill({},{xff}, 0x100) -- ram size is 0x100, 0xff is a OAM garbage
      self.sp_index = 0
      self.sp_addr = 0
      self.sp_latch = 0

      -- for internal state
      -- 8 sprites per line are allowed in standard NES, but a user may remove this limit.
      self.sp_limit = (self.conf.sprite_limit and 8 or 32) * 4
      self.sp_buffer = fill({},{0} , self.sp_limit)
      self.sp_buffered = 0
      self.sp_visible = false
      self.sp_map = fill({},CPU.UNDEFINED, 264) -- [[behind?, zero?, color]]
      self.sp_map_buffer = map(range(0,264),function()  return {false, false, 0} end) -- preallocation for self.sp_map
      self.sp_zero_in_line = false
    end

    function PPU:update_output_color()
      map(range(1,0x20), function(i)
        self.output_color[i] = self.palette[bit.bor(bit.band(self.palette_ram[i] , self.coloring) , self.emphasis)]
      end)
    end

    function PPU:setup_lut()
      self.lut_update ={}--{}.compare_by_identity

      self.name_lut = map(range(0,0xffff), function(i)
        nmt_bank = self.nmt_ref[bit.band(bit.rshift(i , 10) , 3)]
        nmt_idx = bit.band(i , 0x03ff)
        fixed = bit.bor(bit.band(bit.rshift(i , 12), 7) , bit.lshift(i[15] , 12))
        -- WTF
        --(((self.lut_update[nmt_bank] or= [])[nmt_idx] or= [nil, nil])[0] or= []) << [i, fixed]
        return bit.bor(bit.lshift(nmt_bank[nmt_idx] , 4) , fixed)
      end)

      local entries = {}
      self.attr_lut = map(range(0,0x7fff), function(i)
        local io_addr = bit.bor(0x23c0 , bit.bor(bit.band(i , 0x0c00) , bit.bor(bit.band(bit.rshift(i , 4) , 0x0038) , bit.band(bit.rshift(i , 2), 0x0007))))
        local nmt_bank = self.nmt_ref[bit.band(bit.rshift(io_addr , 10) , 3)]
        local nmt_idx = bit.band(io_addr , 0x03ff)
        local attr_shift = bit.bor(bit.band(i , 2) , bit.band(bit.rshift(i , 4) , 4))
        local key = {io_addr, attr_shift}
        entries[key] or= [io_addr, TILE_LUT[bit.band(bit.rshift(nmt_bank[nmt_idx] , attr_shift) , 3)], attr_shift]
        (((self.lut_update[nmt_bank] or= [])[nmt_idx] or= [nil, nil])[1] or= []) << entries[key]
        return entries[key]
      end.freeze
      -- FREZE WTF
      for i=1,#entries do

      end
      entries.each_value {|a| a.uniq! {|entry| entry.object_id } }
    end

    ------------------------------------------------------------------------------------------------------------------------------------------------------
    -- other APIs

    attr_reader :output_pixels

    function PPU:set_chr_mem(mem, writable)
      self.chr_mem = mem
      self.chr_mem_writable = writable
    end

    NMT_TABLE = {
      horizontal:  [0, 0, 1, 1],
      vertical:    [0, 1, 0, 1],
      four_screen: [0, 1, 2, 3],
      first:       [0, 0, 0, 0],
      second:      [1, 1, 1, 1],
    }
    function PPU:nametables=(mode)
      update(PPU.RP2C02_CC)
      idxs = NMT_TABLE[mode]
      return if (0..3).all? {|i| self.nmt_ref[i].equal?(self.nmt_mem[idxs[i]]) }
      self.nmt_ref[0] = self.nmt_mem[idxs[0]]
      self.nmt_ref[1] = self.nmt_mem[idxs[1]]
      self.nmt_ref[2] = self.nmt_mem[idxs[2]]
      self.nmt_ref[3] = self.nmt_mem[idxs[3]]
      setup_lut
    end

    function PPU:update(data_setup)
      sync(data_setup + self.cpu.update)
    end

    function PPU:setup_frame
      self.output_pixels.clear
      self.odd_frame = !self.odd_frame
      self.vclk, self.hclk_target, self.cpu.next_frame_clock = self.hclk == HCLOCK_DUMMY ? DUMMY_FRAME : BOOT_FRAME
    end

    function PPU:vsync
      if self.hclk_target ~= FOREVER_CLOCK
        self.hclk_target = FOREVER_CLOCK
        run
      end
      self.output_pixels << self.palette[15] while self.output_pixels.size < 256 * 240 -- fill black
    end

    function PPU:monitor_a12_rising_edge(monitor)
      self.a12_monitor = monitor
    end

    ------------------------------------------------------------------------------------------------------------------------------------------------------
    -- helpers

    function PPU:update_vram_addr
      if self.vram_addr_inc == 32
        if active?
          if self.scroll_addr_5_14 & 0x7000 == 0x7000
            self.scroll_addr_5_14 &= 0x0fff
            case self.scroll_addr_5_14 & 0x03e0
            when 0x03a0 then self.scroll_addr_5_14 ^= 0x0800
            when 0x03e0 then self.scroll_addr_5_14 &= 0x7c00
            else             self.scroll_addr_5_14 += 0x20
            end
          else
            self.scroll_addr_5_14 += 0x1000
          end
        else
          self.scroll_addr_5_14 += 0x20
        end
      elseif self.scroll_addr_0_4 < 0x1f
        self.scroll_addr_0_4 += 1
      else
        self.scroll_addr_0_4 = 0
        self.scroll_addr_5_14 += 0x20
      end
      update_scroll_address_line
    end

    function PPU:update_scroll_address_line
      self.name_io_addr = (self.scroll_addr_0_4 | self.scroll_addr_5_14) & 0x0fff | 0x2000
      if self.a12_monitor
        a12_state = self.scroll_addr_5_14 & 0x3000 == 0x1000
        self.a12_monitor.a12_signaled(self.cpu.current_clock) if !self.a12_state and a12_state
        self.a12_state = a12_state
      end
    end

    function PPU:active?
      self.scanline ~= SCANLINE_VBLANK and self.any_show
    end

    function PPU:sync(elapsed)
      return unless self.hclk_target < elapsed
      self.hclk_target = elapsed / PPU.RP2C02_CC - self.vclk
      run
    end

    function PPU:make_sure_invariants
      self.name_io_addr = (self.scroll_addr_0_4 | self.scroll_addr_5_14) & 0x0fff | 0x2000
      self.bg_pattern_lut_fetched = TILE_LUT[
        self.nmt_ref[self.io_addr >> 10 & 3][self.io_addr & 0x03ff] >> ((self.scroll_addr_0_4 & 0x2) | (self.scroll_addr_5_14[6] * 0x4)) & 3
      ]
    end

    function PPU:io_latch_mask(data)
      if active?
        0xff
      elseif self.regs_oam & 0x03 == 0x02
        data & 0xe3
      else
        data
      end
    end

    ------------------------------------------------------------------------------------------------------------------------------------------------------
    -- mapped memory handlers

    -- PPUCTRL
    function PPU:poke_2000(_addr, data)
      update(PPU.RP2C02_CC)
      need_nmi_old = self.need_nmi

      self.scroll_latch    = (self.scroll_latch & 0x73ff) | (data & 0x03) << 10
      self.vram_addr_inc   = data[2] == 1 ? 32 : 1
      self.sp_base         = data[3] == 1 ? 0x1000 : 0x0000
      self.bg_pattern_base = data[4] == 1 ? 0x1000 : 0x0000
      self.sp_height       = data[5] == 1 ? 16 : 8
      self.need_nmi        = data[7] == 1

      self.io_latch = data
      self.pattern_end = self.sp_base ~= 0 or self.sp_height == 16 ? 0x1ff0 : 0x0ff0
      self.bg_pattern_base_15 = self.bg_pattern_base[12] << 15

      if self.need_nmi and self.vblank and !need_nmi_old
        clock = self.cpu.current_clock + PPU.RP2C02_CC
        self.cpu.do_nmi(clock) if clock < RP2C02_HVINT
      end
    end

    -- PPUMASK
    function PPU:poke_2001(_addr, data)
      self:update(PPU.RP2C02_CC)
      local bg_show_old, bg_show_edge_old = self.bg_show, self.bg_show_edge
      local sp_show_old, sp_show_edge_old = self.sp_show, self.sp_show_edge
      local any_show_old = self.any_show
      local coloring_old, emphasis_old = self.coloring, self.emphasis

      self.bg_show      = data[3] == 1
      self.bg_show_edge = data[1] == 1 and self.bg_show
      self.sp_show      = data[4] == 1
      self.sp_show_edge = data[2] == 1 and self.sp_show
      self.any_show = self.bg_show or self.sp_show
      self.coloring = data[0] == 1 and 0x30 or 0x3f -- 0x30: monochrome
      self.emphasis = (data & 0xe0) << 1

      self.io_latch = data

      if bg_show_old ~= self.bg_show or bg_show_edge_old ~= self.bg_show_edge or
         sp_show_old ~= self.sp_show or sp_show_edge_old ~= self.sp_show_edge then

        if self.hclk < 8 or self.hclk >= 248 then
          self:update_enabled_flags_edge()
        else
          self:update_enabled_flags()
        end
        if any_show_old and not self.any_show then self:update_scroll_address_line() end
      end

 if coloring_old ~= self.coloring or emphasis_old ~= self.emphasis then self:update_output_color() end
    end

    -- PPUSTATUS
    function PPU:peek_2002(_addr)
      self:update(PPU.RP2C02_CC)
      local v = self.io_latch & 0x1f
      if self.vblank then v |= 0x80 end
      if self.sp_zero_hit then v |= 0x40 end
      if self.sp_overflow then v |= 0x20 end
      self.io_latch = v
      self.scroll_toggle = false
      self.vblanking = false
      self.vblank = false
      return self.io_latch
    end

    -- OAMADDR
    function PPU:poke_2003(_addr, data)
      self:update(PPU.RP2C02_CC)
      self.regs_oam = data
      self.io_latch = data
    end

    -- OAMDATA (write)
    function PPU:poke_2004(_addr, data)
      update(PPU.RP2C02_CC)
      self.sp_ram[self.regs_oam] = io_latch_mask(data)
      self.io_latch = self.sp_ram[self.regs_oam]
      self.regs_oam = bit.band(self.regs_oam + 1, 0xff)
    end

    -- OAMDATA (read)
    function PPU:peek_2004(_addr)
      if notself.any_show or self.cpu.current_clock - (self.cpu.next_frame_clock - (341 * 241) * PPU.RP2C02_CC) >= (341 * 240) * PPU.RP2C02_CC then
        self.io_latch = self.sp_ram[self.regs_oam]
      else
        self:update(PPU.RP2C02_CC)
        self.io_latch = self.sp_latch
      end
    end

    -- PPUSCROLL
    function PPU:poke_2005(_addr, data)
      self:update(PPU.RP2C02_CC)
      self.io_latch = data
      self.scroll_toggle = !self.scroll_toggle
      if self.scroll_toggle then
        self.scroll_latch = self.scroll_latch & 0x7fe0 | (data >> 3)
        xfine = 8 - bit.band(data, 0x7)
        self.bg_pixels.rotate!(self.scroll_xfine - xfine)
        self.scroll_xfine = xfine
      else
        self.scroll_latch = bit.bor(bit.band(self.scroll_latch , 0x0c1f) , bit.bor(bit.lshift(data , 2) , bit.band(bit.lshift(data , 12)) , 0x73e0))
      end
    end

    -- PPUADDR
    function PPU:poke_2006(_addr, data)
      self:update(PPU.RP2C02_CC)
      self.io_latch = data
      self.scroll_toggle = not self.scroll_toggle
      if self.scroll_toggle then
        self.scroll_latch = bit.bor(bit.band(self.scroll_latch , 0x00ff) , bit.lshift(bit.band(data , 0x3f) , 8))
      else
        self.scroll_latch = bit.band(bit.bor(self.scroll_latch , 0x7f00) , data)
        self.scroll_addr_0_4  = bit.band(self.scroll_latch , 0x001f)
        self.scroll_addr_5_14 = bit.band(self.scroll_latch , 0x7fe0)
        self:update_scroll_address_line()
      end
    end

    -- PPUDATA (write)
    function PPU:poke_2007(_addr, data)
      self:update(PPU.RP2C02_CC * 4)
      local addr = bit.bor(self.scroll_addr_0_4 , self.scroll_addr_5_14)
      self:update_vram_addr()
      self.io_latch = data
      if bit.band(addr , 0x3f00) == 0x3f00 then
        addr &= 0x1f
        local final = self.palette[bit.bor(bit.band(data , self.coloring) , self.emphasis)]
        self.palette_ram[addr] = data
        self.output_color[addr] = final
        if bit.band(addr , 3) == 0 then
          self.palette_ram[addr ^ 0x10] = data
          self.output_color[addr ^ 0x10] = final
        end
        self.output_bg_color = bit.band(self.palette_ram[0] , 0x3f
      else
        addr =bit.band*addr, 0x3fff)
        if addr >= 0x2000 then
          nmt_bank = self.nmt_ref[bit.band(bit.rshift(addr , 10) , 0x3)]
          nmt_idx = bit.band(addr , 0x03ff)
          if nmt_bank[nmt_idx] ~= data then
            nmt_bank[nmt_idx] = data

            name_lut_update, attr_lut_update = self.lut_update[nmt_bank][nmt_idx]
             if name_lut_update then name_lut_update.each {|i, b| self.name_lut[i] = data << 4 | b } end
             if attr_lut_update then attr_lut_update.each {|a| a[1] = TILE_LUT[data >> a[2] & 3] } end
          end
        elseif self.chr_mem_writable then
          self.chr_mem[addr] = data
        end
      end
    end

    -- PPUDATA (read)
    function PPU:peek_2007(_addr)
      self:update(PPU.RP2C02_CC)
      addr = (self.scroll_addr_0_4 | self.scroll_addr_5_14) & 0x3fff
      update_vram_addr
      self.io_latch = (addr & 0x3f00) ~= 0x3f00 ? self.io_buffer : self.palette_ram[addr & 0x1f] & self.coloring
      self.io_buffer = addr >= 0x2000 ? self.nmt_ref[addr >> 10 & 0x3][addr & 0x3ff] : self.chr_mem[addr]
      self.io_latch
    end

    function PPU:poke_2xxx(_addr, data)
      self.io_latch = data
    end

    function PPU:peek_2xxx(_addr)
      self.io_latch
    end

    function PPU:peek_3000(_addr)
      update(PPU.RP2C02_CC)
      self.io_latch
    end

    -- OAMDMA
    function PPU:poke_4014(_addr, data) -- DMA
       if self.cpu.odd_clock? then self.cpu.steal_clocks(CPU.CLK_1) end
      self:update(PPU.RP2C02_CC)
      self.cpu.steal_clocks(CPU.CLK_1)
      data =bit.lshift(data, 8)
      if self.regs_oam == 0 and data < 0x2000 and ((not self.any_show) or self.cpu.current_clock <= RP2C02_HVINT - CPU.CLK_1 * 512) then
        self.cpu.steal_clocks(CPU.CLK_1 * 512)
        self.cpu.sprite_dma(bit.band(data , 0x7ff), self.sp_ram)
        self.io_latch = self.sp_ram[0xff]
      else
        repeat
          self.io_latch = self.cpu.fetch(data)
          data =data+ 1
          self.cpu.steal_clocks(CPU.CLK_1)
          elf:update(PPU.RP2C02_CC)
          self.cpu.steal_clocks(CPU.CLK_1)
          self.io_latch = io_latch_mask(self.io_latch)
          self.sp_ram[self.regs_oam] = self.io_latch
          self.regs_oam = bit.band(self.regs_oam + 1, 0xff)
        until bit.band(data , 0xff) ~= 0
      end
    end

    function PPU:peek_4014(_addr)
        return 0x40
    end

    ------------------------------------------------------------------------------------------------------------------------------------------------------
    -- helper methods for PPU--run

    -- NOTE: These methods will be adhocly-inlined.  Keep compatibility with
    -- OptimizedCodeBuilder (e.g., do not change the parameter names blindly).

    function PPU:open_pattern(exp)
      return unless self.any_show
      self.io_addr = exp
      update_address_line
    end

    function PPU:open_sprite(buffer_idx)
      flip_v = self.sp_buffer[buffer_idx + 2][7] -- OAM byte2 bit7: "Flip vertically" flag
      tmp = (self.scanline - self.sp_buffer[buffer_idx]) ^ (flip_v * 0xf)
      byte1 = self.sp_buffer[buffer_idx + 1]
      addr = self.sp_height == 16 ? ((byte1 & 0x01) << 12) | ((byte1 & 0xfe) << 4) | (tmp[3] * 0x10) : self.sp_base | byte1 << 4
      addr | (tmp & 7)
    end

    function PPU:load_sprite(pat0, pat1, buffer_idx)
      byte2 = self.sp_buffer[buffer_idx + 2]
      pos = SP_PIXEL_POSITIONS[byte2[6]] -- OAM byte2 bit6: "Flip horizontally" flag
      pat = (pat0 >> 1 & 0x55) | (pat1 & 0xaa) | ((pat0 & 0x55) | (pat1 << 1 & 0xaa)) << 8
      x_base = self.sp_buffer[buffer_idx + 3]
      palette_base = 0x10 + ((byte2 & 3) << 2) -- OAM byte2 bit0-1: Palette
      self.sp_visible or= self.sp_map.clear
      8.times do |dx|
        x = x_base + dx
        clr = (pat >> (pos[dx] * 2)) & 3
        next if self.sp_map[x] or clr == 0
        self.sp_map[x] = sprite = self.sp_map_buffer[x]
        -- sprite[0]: behind flag, sprite[1]: zero hit flag, sprite[2]: color
        sprite[0] = byte2[5] == 1 -- OAM byte2 bit5: "Behind background" flag
        sprite[1] = buffer_idx == 0 and self.sp_zero_in_line
        sprite[2] = palette_base + clr
      end
      self.sp_active = self.sp_enabled
    end

    function PPU:update_address_line
      if self.a12_monitor
        a12_state = self.io_addr[12] == 1
        self.a12_monitor.a12_signaled((self.vclk + self.hclk) * PPU.RP2C02_CC) if !self.a12_state and a12_state
        self.a12_state = a12_state
      end
    end

    ------------------------------------------------------------------------------------------------------------------------------------------------------
    -- actions for PPU--run

    function PPU:open_name
      return unless self.any_show
      self.io_addr = self.name_io_addr
      update_address_line
    end

    function PPU:fetch_name
      return unless self.any_show
      self.io_pattern = self.name_lut[self.scroll_addr_0_4 + self.scroll_addr_5_14 + self.bg_pattern_base_15]
    end

    function PPU:open_attr
      return unless self.any_show
      self.io_addr, self.bg_pattern_lut_fetched, = self.attr_lut[self.scroll_addr_0_4 + self.scroll_addr_5_14]
      update_address_line
    end

    function PPU:fetch_attr
      return unless self.any_show
      self.bg_pattern_lut = self.bg_pattern_lut_fetched
      -- raise unless self.bg_pattern_lut_fetched ==
      --   self.nmt_ref[self.io_addr >> 10 & 3][self.io_addr & 0x03ff] >>
      --     ((self.scroll_addr_0_4 & 0x2) | (self.scroll_addr_5_14[6] * 0x4)) & 3
    end

    function PPU:fetch_bg_pattern_0
      return unless self.any_show
      self.bg_pattern = self.chr_mem[self.io_addr & 0x1fff]
    end

    function PPU:fetch_bg_pattern_1
      return unless self.any_show
      self.bg_pattern |= self.chr_mem[self.io_addr & 0x1fff] * 0x100
    end

    function PPU:scroll_clock_x
      return unless self.any_show
      if self.scroll_addr_0_4 < 0x001f
        self.scroll_addr_0_4 += 1
        self.name_io_addr += 1 -- make cache consistent
      else
        self.scroll_addr_0_4 = 0
        self.scroll_addr_5_14 ^= 0x0400
        self.name_io_addr ^= 0x041f -- make cache consistent
      end
    end

    function PPU:scroll_reset_x
      return unless self.any_show
      self.scroll_addr_0_4 = self.scroll_latch & 0x001f
      self.scroll_addr_5_14 = (self.scroll_addr_5_14 & 0x7be0) | (self.scroll_latch & 0x0400)
      self.name_io_addr = (self.scroll_addr_0_4 | self.scroll_addr_5_14) & 0x0fff | 0x2000 -- make cache consistent
    end

    function PPU:scroll_clock_y
      return unless self.any_show
      if self.scroll_addr_5_14 & 0x7000 ~= 0x7000
        self.scroll_addr_5_14 += 0x1000
      else
        mask = self.scroll_addr_5_14 & 0x03e0
        if mask == 0x03a0
          self.scroll_addr_5_14 ^= 0x0800
          self.scroll_addr_5_14 &= 0x0c00
        elseif mask == 0x03e0
          self.scroll_addr_5_14 &= 0x0c00
        else
          self.scroll_addr_5_14 = (self.scroll_addr_5_14 & 0x0fe0) + 32
        end
      end

      self.name_io_addr = (self.scroll_addr_0_4 | self.scroll_addr_5_14) & 0x0fff | 0x2000 -- make cache consistent
    end

    function PPU:preload_tiles
      return unless self.any_show
      self.bg_pixels[self.scroll_xfine, 8] = self.bg_pattern_lut[self.bg_pattern]
    end

    function PPU:load_tiles
      return unless self.any_show
      self.bg_pixels.rotate!(8)
      self.bg_pixels[self.scroll_xfine, 8] = self.bg_pattern_lut[self.bg_pattern]
    end

    function PPU:evaluate_sprites_even
      return unless self.any_show
      self.sp_latch = self.sp_ram[self.sp_addr]
    end

    function PPU:evaluate_sprites_odd
      return unless self.any_show

      -- we first check phase 1 since it is the most-likely case
      if self.sp_phase -- nil represents phase 1
        -- the second most-likely case is phase 9
        if self.sp_phase == 9
          evaluate_sprites_odd_phase_9
        else
          -- other cases are relatively rare
          case self.sp_phase
          -- when 1 then evaluate_sprites_odd_phase_1
          -- when 9 then evaluate_sprites_odd_phase_9
          when 2 then evaluate_sprites_odd_phase_2
          when 3 then evaluate_sprites_odd_phase_3
          when 4 then evaluate_sprites_odd_phase_4
          when 5 then evaluate_sprites_odd_phase_5
          when 6 then evaluate_sprites_odd_phase_6
          when 7 then evaluate_sprites_odd_phase_7
          when 8 then evaluate_sprites_odd_phase_8
          end
        end
      else
        evaluate_sprites_odd_phase_1
      end
    end

    function PPU:evaluate_sprites_odd_phase_1
      self.sp_index += 1
      if self.sp_latch <= self.scanline and self.scanline < self.sp_latch + self.sp_height
        self.sp_addr += 1
        self.sp_phase = 2
        self.sp_buffer[self.sp_buffered] = self.sp_latch
      elseif self.sp_index == 64
        self.sp_addr = 0
        self.sp_phase = 9
      elseif self.sp_index == 2
        self.sp_addr = 8
      else
        self.sp_addr += 4
      end
    end

    function PPU:evaluate_sprites_odd_phase_2
      self.sp_addr += 1
      self.sp_phase = 3
      self.sp_buffer[self.sp_buffered + 1] = self.sp_latch
    end

    function PPU:evaluate_sprites_odd_phase_3
      self.sp_addr += 1
      self.sp_phase = 4
      self.sp_buffer[self.sp_buffered + 2] = self.sp_latch
    end

    function PPU:evaluate_sprites_odd_phase_4
      self.sp_buffer[self.sp_buffered + 3] = self.sp_latch
      self.sp_buffered += 4
      if self.sp_index ~= 64
        self.sp_phase = self.sp_buffered ~= self.sp_limit ? nil : 5
        if self.sp_index ~= 2
          self.sp_addr += 1
          self.sp_zero_in_line or= self.sp_index == 1
        else
          self.sp_addr = 8
        end
      else
        self.sp_addr = 0
        self.sp_phase = 9
      end
    end

    function PPU:evaluate_sprites_odd_phase_5
      if self.sp_latch <= self.scanline and self.scanline < self.sp_latch + self.sp_height
        self.sp_phase = 6
        self.sp_addr = (self.sp_addr + 1) & 0xff
        self.sp_overflow = true
      else
        self.sp_addr = ((self.sp_addr + 4) & 0xfc) + ((self.sp_addr + 1) & 3)
        if self.sp_addr <= 5
          self.sp_phase = 9
          self.sp_addr &= 0xfc
        end
      end
    end

    function PPU:evaluate_sprites_odd_phase_6
      self.sp_phase = 7
      self.sp_addr = (self.sp_addr + 1) & 0xff
    end

    function PPU:evaluate_sprites_odd_phase_7
      self.sp_phase = 8
      self.sp_addr = (self.sp_addr + 1) & 0xff
    end

    function PPU:evaluate_sprites_odd_phase_8
      self.sp_phase = 9
      self.sp_addr = (self.sp_addr + 1) & 0xff
      self.sp_addr += 1 if self.sp_addr & 3 == 3
      self.sp_addr &= 0xfc
    end

    function PPU:evaluate_sprites_odd_phase_9
      self.sp_addr = (self.sp_addr + 4) & 0xff
    end

    function PPU:load_extended_sprites
      return unless self.any_show
      if 32 < self.sp_buffered
        buffer_idx = 32
        begin
          addr = open_sprite(buffer_idx)
          pat0 = self.chr_mem[addr]
          pat1 = self.chr_mem[addr | 8]
          load_sprite(pat0, pat1, buffer_idx) if pat0 ~= 0 or pat1 ~= 0
          buffer_idx += 4
        end while buffer_idx ~= self.sp_buffered
      end
    end

    function PPU:render_pixel
      if self.any_show
        pixel = self.bg_enabled ? self.bg_pixels[self.hclk % 8] : 0
        if self.sp_active and (sprite = self.sp_map[self.hclk])
          if pixel % 4 == 0
            pixel = sprite[2]
          else
            self.sp_zero_hit = true if sprite[1] and self.hclk ~= 255
            pixel = sprite[2] unless sprite[0]
          end
        end
      else
        pixel = self.scroll_addr_5_14 & 0x3f00 == 0x3f00 ? self.scroll_addr_0_4 : 0
        self.bg_pixels[self.hclk % 8] = 0
      end
      self.output_pixels << self.output_color[pixel]
    end

    -- just a placeholder; used for batch_render_pixels optimization
    function PPU:batch_render_eight_pixels
    end

    function PPU:boot
      self.vblank = true
      self.hclk = HCLOCK_DUMMY
      self.hclk_target = FOREVER_CLOCK
    end

    function PPU:vblank_0
      self.vblanking = true
      self.hclk = HCLOCK_VBLANK_1
    end

    function PPU:vblank_1
      self.vblank or= self.vblanking
      self.vblanking = false
      self.sp_visible = false
      self.sp_active = false
      self.hclk = HCLOCK_VBLANK_2
    end

    function PPU:vblank_2
      self.vblank or= self.vblanking
      self.vblanking = false
      self.hclk = HCLOCK_DUMMY
      self.hclk_target = FOREVER_CLOCK
      self.cpu.do_nmi(self.cpu.next_frame_clock) if self.need_nmi and self.vblank
    end

    function PPU:update_enabled_flags
      return unless self.any_show
      self.bg_enabled = self.bg_show
      self.sp_enabled = self.sp_show
      self.sp_active = self.sp_enabled and self.sp_visible
    end

    function PPU:update_enabled_flags_edge
      self.bg_enabled = self.bg_show_edge
      self.sp_enabled = self.sp_show_edge
      self.sp_active = self.sp_enabled and self.sp_visible
    end

    ------------------------------------------------------------------------------------------------------------------------------------------------------
    -- default core

    function PPU:debug_logging(scanline, hclk, hclk_target)
      hclk = "forever" if hclk == FOREVER_CLOCK
      hclk_target = "forever" if hclk_target == FOREVER_CLOCK

      self.conf.debug("ppu: scanline --{ scanline }, hclk --{ hclk }->--{ hclk_target }")
    end

    function PPU:run
      self.fiber or= Fiber.new { main_loop }

      debug_logging(self.scanline, self.hclk, self.hclk_target) if self.conf.loglevel >= 3

      make_sure_invariants

      self.hclk_target = (self.vclk + self.hclk) * PPU.RP2C02_CC unless self.fiber.resume
    end

    function PPU:wait_frame
      Fiber.yield true
    end

    function PPU:wait_zero_clocks
      Fiber.yield if self.hclk_target <= self.hclk
    end

    function PPU:wait_one_clock
      self.hclk += 1
      Fiber.yield if self.hclk_target <= self.hclk
    end

    function PPU:wait_two_clocks
      self.hclk += 2
      Fiber.yield if self.hclk_target <= self.hclk
    end

    ------ main-loop structure
    --
    -- -- wait for boot
    -- clk_685
    --
    -- loop do
    --   -- pre-render scanline
    --   clk_341, clk_342, ..., clk_659
    --   while true
    --     -- visible scanline (not shown)
    --     clk_320, clk_321, ..., clk_337
    --
    --     -- increment scanline
    --     clk_338
    --     break if self.scanline == 240
    --
    --     -- visible scanline (shown)
    --     clk_0, clk_1, ..., clk_319
    --   end
    --
    --   -- post-render sacnline (vblank)
    --   do_681,682,684
    -- end
    --
    -- This method definition also serves as a template for OptimizedCodeBuilder.
    -- Comments like "when NNN" are markers for the purpose.
    --
    -- rubocop:disable Metrics/MethodLength, Metrics/CyclomaticComplexity, Metrics/PerceivedComplexity, Metrics/AbcSize
    function PPU:main_loop
      -- when 685

      -- wait for boot
      boot
      wait_frame

      while true
        -- pre-render scanline

        341.step(589, 8) do
          -- when 341, 349, ..., 589
          if self.hclk == 341
            self.sp_overflow = self.sp_zero_hit = self.vblanking = self.vblank = false
            self.scanline = SCANLINE_HDUMMY
          end
          open_name
          wait_two_clocks

          -- when 343, 351, ..., 591
          open_attr
          wait_two_clocks

          -- when 345, 353, ..., 593
          open_pattern(self.bg_pattern_base)
          wait_two_clocks

          -- when 347, 355, ..., 595
          open_pattern(self.io_addr | 8)
          wait_two_clocks
        end

        597.step(653, 8) do
          -- when 597, 605, ..., 653
          if self.any_show
            if self.hclk == 645
              self.scroll_addr_0_4  = self.scroll_latch & 0x001f
              self.scroll_addr_5_14 = self.scroll_latch & 0x7fe0
              self.name_io_addr = (self.scroll_addr_0_4 | self.scroll_addr_5_14) & 0x0fff | 0x2000 -- make cache consistent
            end
          end
          open_name
          wait_two_clocks

          -- when 599, 607, ..., 655
          -- Nestopia uses open_name here?
          open_attr
          wait_two_clocks

          -- when 601, 609, ..., 657
          open_pattern(self.pattern_end)
          wait_two_clocks

          -- when 603, 611, ..., 659
          open_pattern(self.io_addr | 8)
          if self.hclk == 659
            self.hclk = 320
            self.vclk += HCLOCK_DUMMY
            self.hclk_target -= HCLOCK_DUMMY
          else
            wait_two_clocks
          end
          wait_zero_clocks
        end

        while true
          -- visible scanline (not shown)

          -- when 320
          load_extended_sprites
          open_name
          self.sp_latch = self.sp_ram[0] if self.any_show
          self.sp_buffered = 0
          self.sp_zero_in_line = false
          self.sp_index = 0
          self.sp_phase = 0
          wait_one_clock

          -- when 321
          fetch_name
          wait_one_clock

          -- when 322
          open_attr
          wait_one_clock

          -- when 323
          fetch_attr
          scroll_clock_x
          wait_one_clock

          -- when 324
          open_pattern(self.io_pattern)
          wait_one_clock

          -- when 325
          fetch_bg_pattern_0
          wait_one_clock

          -- when 326
          open_pattern(self.io_pattern | 8)
          wait_one_clock

          -- when 327
          fetch_bg_pattern_1
          wait_one_clock

          -- when 328
          preload_tiles
          open_name
          wait_one_clock

          -- when 329
          fetch_name
          wait_one_clock

          -- when 330
          open_attr
          wait_one_clock

          -- when 331
          fetch_attr
          scroll_clock_x
          wait_one_clock

          -- when 332
          open_pattern(self.io_pattern)
          wait_one_clock

          -- when 333
          fetch_bg_pattern_0
          wait_one_clock

          -- when 334
          open_pattern(self.io_pattern | 8)
          wait_one_clock

          -- when 335
          fetch_bg_pattern_1
          wait_one_clock

          -- when 336
          open_name
          wait_one_clock

          -- when 337
          if self.any_show
            update_enabled_flags_edge
            self.cpu.next_frame_clock = RP2C02_HVSYNC_1 if self.scanline == SCANLINE_HDUMMY and self.odd_frame
          end
          wait_one_clock

          -- when 338
          open_name
          self.scanline += 1
          if self.scanline ~= SCANLINE_VBLANK
            if self.any_show
              line = self.scanline ~= 0 or !self.odd_frame ? 341 : 340
            else
              update_enabled_flags_edge
              line = 341
            end
            self.hclk = 0
            self.vclk += line
            self.hclk_target = self.hclk_target <= line ? 0 : self.hclk_target - line
          else
            self.hclk = HCLOCK_VBLANK_0
            wait_zero_clocks
            break
          end
          wait_zero_clocks

          -- visible scanline (shown)
          0.step(248, 8) do
            -- when 0, 8, ..., 248
            if self.any_show
              if self.hclk == 64
                self.sp_addr = self.regs_oam & 0xf8 -- SP_OFFSET_TO_0_1
                self.sp_phase = nil
                self.sp_latch = 0xff
              end
              load_tiles
              batch_render_eight_pixels
              evaluate_sprites_even if self.hclk >= 64
              open_name
            end
            render_pixel
            wait_one_clock

            -- when 1, 9, ..., 249
            if self.any_show
              fetch_name
              evaluate_sprites_odd if self.hclk >= 64
            end
            render_pixel
            wait_one_clock

            -- when 2, 10, ..., 250
            if self.any_show
              evaluate_sprites_even if self.hclk >= 64
              open_attr
            end
            render_pixel
            wait_one_clock

            -- when 3, 11, ..., 251
            if self.any_show
              fetch_attr
              evaluate_sprites_odd if self.hclk >= 64
              scroll_clock_y if self.hclk == 251
              scroll_clock_x
            end
            render_pixel
            wait_one_clock

            -- when 4, 12, ..., 252
            if self.any_show
              evaluate_sprites_even if self.hclk >= 64
              open_pattern(self.io_pattern)
            end
            render_pixel
            wait_one_clock

            -- when 5, 13, ..., 253
            if self.any_show
              fetch_bg_pattern_0
              evaluate_sprites_odd if self.hclk >= 64
            end
            render_pixel
            wait_one_clock

            -- when 6, 14, ..., 254
            if self.any_show
              evaluate_sprites_even if self.hclk >= 64
              open_pattern(self.io_pattern | 8)
            end
            render_pixel
            wait_one_clock

            -- when 7, 15, ..., 255
            if self.any_show
              fetch_bg_pattern_1
              evaluate_sprites_odd if self.hclk >= 64
            end
            render_pixel
            -- rubocop:disable Style/NestedModifier, Style/IfUnlessModifierOfIfUnless:
            update_enabled_flags if self.hclk ~= 255 if self.any_show
            -- rubocop:enable Style/NestedModifier, Style/IfUnlessModifierOfIfUnless:
            wait_one_clock
          end

          256.step(312, 8) do
            -- rubocop:disable Style/IdenticalConditionalBranches
            if self.hclk == 256
              -- when 256
              open_name
              self.sp_latch = 0xff if self.any_show
              wait_one_clock

              -- when 257
              scroll_reset_x
              self.sp_visible = false
              self.sp_active = false
              wait_one_clock
            else
              -- when 264, 272, ..., 312
              open_name
              wait_two_clocks
            end
            -- rubocop:enable Style/IdenticalConditionalBranches

            -- when 258, 266, ..., 314
            -- Nestopia uses open_name here?
            open_attr
            wait_two_clocks

            -- when 260, 268, ..., 316
            if self.any_show
              buffer_idx = (self.hclk - 260) / 2
              open_pattern(buffer_idx >= self.sp_buffered ? self.pattern_end : open_sprite(buffer_idx))
              -- rubocop:disable Style/NestedModifier, Style/IfUnlessModifierOfIfUnless:
              self.regs_oam = 0 if self.scanline == 238 if self.hclk == 316
              -- rubocop:enable Style/NestedModifier, Style/IfUnlessModifierOfIfUnless:
            end
            wait_one_clock

            -- when 261, 269, ..., 317
            if self.any_show
              self.io_pattern = self.chr_mem[self.io_addr & 0x1fff] if (self.hclk - 261) / 2 < self.sp_buffered
            end
            wait_one_clock

            -- when 262, 270, ..., 318
            open_pattern(self.io_addr | 8)
            wait_one_clock

            -- when 263, 271, ..., 319
            if self.any_show
              buffer_idx = (self.hclk - 263) / 2
              if buffer_idx < self.sp_buffered
                pat0 = self.io_pattern
                pat1 = self.chr_mem[self.io_addr & 0x1fff]
                load_sprite(pat0, pat1, buffer_idx) if pat0 ~= 0 or pat1 ~= 0
              end
            end
            wait_one_clock
          end
        end

        -- post-render scanline (vblank)

        -- when 681
        vblank_0
        wait_zero_clocks

        -- when 682
        vblank_1
        wait_zero_clocks

        -- when 684
        vblank_2
        wait_frame
      end
    end
    -- rubocop:enable Metrics/MethodLength, Metrics/CyclomaticComplexity, Metrics/PerceivedComplexity, Metrics/AbcSize

    ------------------------------------------------------------------------------------------------------------------------------------------------------
    -- optimized core generator
    class OptimizedCodeBuilder
      include CodeOptimizationHelper

      OPTIONS = [
        :method_inlining, :ivar_localization,
        :split_show_mode, :split_a12_checks, :clock_specialization,
        :fastpath, :batch_render_pixels, :oneline,
      ]

      function PPU:build
        depends(:ivar_localization, :method_inlining)
        depends(:batch_render_pixels, :fastpath)

        mdefs = parse_method_definitions(__FILE__)
        handlers = parse_clock_handlers(mdefs[:main_loop].body)

        handlers = specialize_clock_handlers(handlers) if self.clock_specialization
        if self.fastpath
          handlers = add_fastpath(handlers) do |fastpath, hclk|
            self.batch_render_pixels ? batch_render_pixels(fastpath, hclk) : fastpath
          end
        end
        code = build_loop(handlers)
        code = ppu_expand_methods(code, mdefs) if self.method_inlining

        if self.split_show_mode
          code, code_no_show = split_mode(code, "self.any_show")
          if self.split_a12_checks
            code, code_no_a12 = split_mode(code, "self.a12_monitor")
            code = branch("self.a12_monitor", code, code_no_a12)
          end
          code = branch("self.any_show", code, code_no_show)
        end

        code = gen(
          mdefs[:make_sure_invariants].body,
          code,
          "self.hclk_target = (self.vclk + self.hclk) * PPU.RP2C02_CC"
        )

        code = localize_instance_variables(code) if self.ivar_localization

        code = gen(
          "function PPU:self.run",
          *(self.loglevel >= 3 ? ["  debug_logging(self.scanline, self.hclk, self.hclk_target)"] : []),
          indent(2, code),
          "end",
        )

        code = oneline(code) if self.oneline

        code
      end

      COMMANDS = {
        wait_zero_clocks: "",
        wait_one_clock:   "self.hclk += 1\n",
        wait_two_clocks:  "self.hclk += 2\n",
        wait_frame:       "return\n",
      }

      -- extracts the actions for each clock from CPU--main_loop
      function PPU:parse_clock_handlers(main_loop)
        handlers = {}
        main_loop.scan(/^( *)-- when (.*)\n((?:\1.*\n|\n)*?\1wait_.*\n)/) do |indent, hclks, body|
          body = indent(-indent.size, body)
          body = body.gsub(/^( *)break\n/, "")
          body = expand_methods(body, COMMANDS)
          if hclks =~ /^(\d+), (\d+), \.\.\., (\d+)$/
            first, second, last = $1.to_i, $2.to_i, $3.to_i
            first.step(last, second - first) do |hclk|
              handlers[hclk] = body
            end
          else
            handlers[hclks.to_i] = body
          end
        end
        handlers
      end

      -- split clock handlers that contains a branch depending on clock
      function PPU:specialize_clock_handlers(handlers)
        handlers.each do |hclk, handler|
          -- pre-caluculate some conditions like `self.hclk == 64` with `false`
          handler = handler.gsub(/self.hclk (==|>=|~=) (\d+)/) { hclk.send($1.to_sym, $2.to_i) }

          -- remove disabled branches like `if false ... end`
          handlers[hclk] = remove_trivial_branches(handler)
        end
      end

      -- pass a fastpath
      function PPU:add_fastpath(handlers)
        handlers.each do |hclk, handler|
          next unless hclk % 8 == 0 and hclk < 256
          fastpath = gen(*(0..7).map {|i| handlers[hclk + i] })
          fastpath = yield fastpath, hclk
          handlers[hclk] = branch("self.hclk + 8 <= self.hclk_target", fastpath, handler)
        end
      end

      -- replace eight `render_pixel` calls with one optimized batch version
      function PPU:batch_render_pixels(fastpath, hclk)
        fastpath = expand_methods(fastpath, render_pixel: gen(
          "unless self.any_show",
          "  self.bg_pixels[self.hclk % 8] = 0",
          "  self.output_pixels << self.output_color[self.scroll_addr_5_14 & 0x3f00 == 0x3f00 ? self.scroll_addr_0_4 : 0]",
          "end",
        ))
        expand_methods(fastpath, batch_render_eight_pixels: gen(
          "-- batch-version of render_pixel",
          "if self.any_show",
          "  if self.sp_active",
          "    if self.bg_enabled",
          *(0..7).flat_map do |i|
            [
              "      pixel--{ i } = self.bg_pixels[--{ i }]",
              "      if sprite = self.sp_map[self.hclk--{ i ~= 0 ? " + --{ i }" : "" }]",
              "        if pixel--{ i } % 4 == 0",
              "          pixel--{ i } = sprite[2]",
              "        else",
              *(hclk + i == 255 ? [] : ["          self.sp_zero_hit = true if sprite[1]"]),
              "          pixel--{ i } = sprite[2] unless sprite[0]",
              "        end",
              "      end",
            ]
          end,
          "      self.output_pixels << " + (0..7).map {|n| "self.output_color[pixel--{ n }]" } * " << ",
          "    else",
          *(0..7).map do |i|
            "      pixel--{ i } = (sprite = self.sp_map[self.hclk --{ i ~= 0 ? " + --{ i }" : "" }]) ? sprite[2] : 0"
          end,
          "      self.output_pixels << " + (0..7).map {|n| "self.output_color[pixel--{ n }]" } * " << ",
          "    end",
          "  else",
          "    if self.bg_enabled -- this is the true hot-spot",
          "      self.output_pixels << " + (0..7).map {|n| "self.output_color[self.bg_pixels[--{ n }]]" } * " << ",
          "    else",
          "      clr = self.output_color[0]",
          "      self.output_pixels << " + ["clr"] * 8 * " << ",
          "    end",
          "  end",
          "end",
        ))
      end

      -- remove all newlines (this will reduce `trace` instructions)
      function PPU:oneline(code)
        code.gsub(/^ *|--.*/, "").gsub("[\n", "[").gsub(/\n *\]/, "]").tr("\n", ";")
      end

      -- inline method calls
      function PPU:ppu_expand_methods(code, mdefs)
        code = expand_inline_methods(code, :open_sprite, mdefs[:open_sprite])

        -- twice is enough
        expand_methods(expand_methods(code, mdefs), mdefs)
      end

      -- create two version of the same code by evaluating easy branches
      -- CAUTION: the condition must be invariant during PPU--run
      function PPU:split_mode(code, cond)
        %w(true false).map do |bool|
          rebuild_loop(remove_trivial_branches(replace_cond_var(code, cond, bool)))
        end
      end

      -- generate a main code
      function PPU:build_loop(handlers)
        clauses = {}
        handlers.sort.each do |hclk, handler|
          (clauses[handler] or= []) << hclk
        end

        gen(
          "while self.hclk_target > self.hclk",
          "  case self.hclk",
          *clauses.invert.sort.map do |hclks, handler|
            "  when --{ hclks * ", " }\n" + indent(4, handler)
          end,
          "  end",
          "end",
        )
      end

      -- deconstruct a loop, unify handlers, and re-generate a new loop
      function PPU:rebuild_loop(code)
        handlers = {}
        code.scan(/^  when ((?:\d+, )*\d+)\n((?:    .*\n|\n)*)/) do |hclks, handler|
          hclks.split(", ").each do |hclk|
            handlers[hclk.to_i] = indent(-4, handler)
          end
        end
        build_loop(handlers)
      end
    end
  end
end
