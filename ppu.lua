local band, bor, bxor, bnot, lshift, rshift = bit.band, bit.bor, bit.bxor, bit.bnot, bit.lshift, bit.rshift
local map, rotatePositiveIdx, nthBitIsSet, nthBitIsSetInt, range =
    UTILS.map,
    UTILS.rotatePositiveIdx,
    UTILS.nthBitIsSet,
    UTILS.nthBitIsSetInt,
    UTILS.range

PPU = {}
local PPU = PPU
PPU._mt = { __index = PPU }
function PPU:new(conf, cpu, palette)
    local ppu = {}
    setmetatable(ppu, PPU._mt)
    ppu:initialize(conf, cpu, palette)
    return ppu
end

PPU.SCREEN_HEIGHT = 240
PPU.SCREEN_WIDTH = 256
-- clock/timing constants (stolen from Nestopia)
PPU.RP2C02_CC = 4
PPU.RP2C02_HACTIVE = PPU.RP2C02_CC * 256
PPU.RP2C02_HBLANK = PPU.RP2C02_CC * 85
PPU.RP2C02_HSYNC = PPU.RP2C02_HACTIVE + PPU.RP2C02_HBLANK
PPU.RP2C02_VACTIVE = 240
PPU.RP2C02_VSLEEP = 1
PPU.RP2C02_VINT = 20
PPU.RP2C02_VDUMMY = 1
PPU.RP2C02_VBLANK = PPU.RP2C02_VSLEEP + PPU.RP2C02_VINT + PPU.RP2C02_VDUMMY
PPU.RP2C02_VSYNC = PPU.RP2C02_VACTIVE + PPU.RP2C02_VBLANK
PPU.RP2C02_HVSYNCBOOT = PPU.RP2C02_VACTIVE * PPU.RP2C02_HSYNC + PPU.RP2C02_CC * 312
PPU.RP2C02_HVINT = PPU.RP2C02_VINT * PPU.RP2C02_HSYNC
PPU.RP2C02_HVSYNC_0 = PPU.RP2C02_VSYNC * PPU.RP2C02_HSYNC
PPU.RP2C02_HVSYNC_1 = PPU.RP2C02_VSYNC * PPU.RP2C02_HSYNC - PPU.RP2C02_CC

-- special scanlines
PPU.SCANLINE_HDUMMY = -1  -- pre-render scanline
PPU.SCANLINE_VBLANK = 240 -- post-render scanline

-- special horizontal clocks
PPU.HCLOCK_DUMMY = 341
PPU.HCLOCK_VBLANK_0 = 681
PPU.HCLOCK_VBLANK_1 = 682
PPU.HCLOCK_VBLANK_2 = 684
PPU.HCLOCK_BOOT = 685
PPU.DUMMY_FRAME = { PPU.RP2C02_HVINT / PPU.RP2C02_CC - PPU.HCLOCK_DUMMY, PPU.RP2C02_HVINT, PPU.RP2C02_HVSYNC_0 }
PPU.BOOT_FRAME = {
    PPU.RP2C02_HVSYNCBOOT / PPU.RP2C02_CC - PPU.HCLOCK_BOOT,
    PPU.RP2C02_HVSYNCBOOT,
    PPU.RP2C02_HVSYNCBOOT
}

-- constants related to OAM (sprite)
PPU.SP_PIXEL_POSITIONS = {
    { 3, 7, 2, 6, 1, 5, 0, 4 }, -- normal
    { 4, 0, 5, 1, 6, 2, 7, 3 }  -- flip
}

-- A look-up table mapping: (two pattern bytes * attr) -> eight pixels
--   TILE_LUT[attr][high_byte * 0x100 + low_byte] = [pixels] * 8
PPU.TILE_LUT =
    map(
        { 0x0, 0x4, 0x8, 0xc },
        function(attr)
            return UTILS.transpose(
                map(
                    range(0, 7),
                    function(j)
                        return map(
                            range(0, 0x10000),
                            function(i)
                                local clr = nthBitIsSetInt(i, 15 - j) * 2 + nthBitIsSetInt(i, 7 - j)
                                return clr ~= 0 and bor(attr, clr) or 0
                            end
                        )
                    end
                )
            )
        end
    )
local TILE_LUT,
HCLOCK_BOOT,
HCLOCK_DUMMY,
BOOT_FRAME,
DUMMY_FRAME,
FOREVER_CLOCK,
SCANLINE_VBLANK,
SCANLINE_HDUMMY,
RP2C02_CC =
    PPU.TILE_LUT,
    PPU.HCLOCK_BOOT,
    PPU.HCLOCK_DUMMY,
    PPU.BOOT_FRAME,
    PPU.DUMMY_FRAME,
    CPU.FOREVER_CLOCK,
    PPU.SCANLINE_VBLANK,
    PPU.SCANLINE_HDUMMY,
    PPU.RP2C02_CC
--------------------------------------------------------------------------------------------------------------------
-- initialization
local any_show
function PPU:initialize(conf, cpu, palette)
    self.conf = conf
    self.cpu = cpu
    self.palette = palette

    self.scanline_counter_target = 0
    self.inform_scanline_counter = false
    self.scanline_counter_callback = function() end
    self.on_scanline_240 = function() end
    self.on_scanline_0 = function() end

    self.nmt_mem = { [0] = UTILS.fill({}, 0xff, 0x400, 1, -1), [1] = UTILS.fill({}, 0xff, 0x400, 1, -1) }
    --[  [0xff] * 0x400, [0xff] * 0x400]
    self.nmt_ref =
        map(
            { [0] = 0, [1] = 1, [2] = 0, [3] = 1 },
            function(i)
                return self.nmt_mem[i]
            end
        )

    --self.output_pixels = {}
    self.output_pixels = UTILS.fill({}, self.palette[16], PPU.SCREEN_HEIGHT * PPU.SCREEN_WIDTH)

    self.output_pixels_size = 0
    self.output_color = UTILS.fill({}, { self.palette[1] }, 0x20) -- palette size is 0x20
    self:reset(true)
    self:setup_lut()
end

function PPU:scanline_counter_listener(scanline_counter_target, callback)
    self.scanline_counter_target = scanline_counter_target
    self.scanline_counter_callback = callback
end

function PPU:reset(mapping)
    self.nt_cache        = { nmt_ref = {} }

    self.chr_poke        = function(i, v)
        self.chr_mem[i + 1] = v
    end
    self.chr_peek        = function(i)
        return self.chr_mem[i + 1]
    end
    self.chr_bg_read     = self.chr_peek
    self.chr_sprite_read = self.chr_peek

    if mapping or true then
        -- setup mapped memory
        self.cpu:add_mappings(
            range(0x2000, 0x3fff, 8),
            UTILS.bind(self.peek_2xxx, self),
            UTILS.bind(self.poke_2000, self)
        )
        self.cpu:add_mappings(
            range(0x2001, 0x3fff, 8),
            UTILS.bind(self.peek_2xxx, self),
            UTILS.bind(self.poke_2001, self)
        )
        self.cpu:add_mappings(
            range(0x2002, 0x3fff, 8),
            UTILS.bind(self.peek_2002, self),
            UTILS.bind(self.poke_2xxx, self)
        )
        self.cpu:add_mappings(
            range(0x2003, 0x3fff, 8),
            UTILS.bind(self.peek_2xxx, self),
            UTILS.bind(self.poke_2003, self)
        )
        self.cpu:add_mappings(
            range(0x2004, 0x3fff, 8),
            UTILS.bind(self.peek_2004, self),
            UTILS.bind(self.poke_2004, self)
        )
        self.cpu:add_mappings(
            range(0x2005, 0x3fff, 8),
            UTILS.bind(self.peek_2xxx, self),
            UTILS.bind(self.poke_2005, self)
        )
        self.cpu:add_mappings(
            range(0x2006, 0x3fff, 8),
            UTILS.bind(self.peek_2xxx, self),
            UTILS.bind(self.poke_2006, self)
        )
        self.cpu:add_mappings(
            range(0x2007, 0x3fff, 8),
            UTILS.bind(self.peek_2007, self),
            UTILS.bind(self.poke_2007, self)
        )
        self.cpu:add_mappings(0x3000, UTILS.bind(self.peek_3000, self), UTILS.bind(self.poke_2000, self))
        self.cpu:add_mappings(0x4014, UTILS.bind(self.peek_4014, self), UTILS.bind(self.poke_4014, self))
    end

    self.palette_ram = {
        0x3f,
        0x01,
        0x00,
        0x01,
        0x00,
        0x02,
        0x02,
        0x0d,
        0x08,
        0x10,
        0x08,
        0x24,
        0x00,
        0x00,
        0x04,
        0x2c,
        0x09,
        0x01,
        0x34,
        0x03,
        0x00,
        0x04,
        0x00,
        0x14,
        0x08,
        0x3a,
        0x00,
        0x02,
        0x00,
        0x20,
        0x2c,
        0x08
    }
    self.coloring = 0x3f -- not monochrome
    self.emphasis = 0
    self:update_output_color()

    -- clock management
    self.hclk = HCLOCK_BOOT
    self.vclk = 0
    self.hclk_target = FOREVER_CLOCK

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
    self.scanline = SCANLINE_VBLANK

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
    self.bg_pixels = UTILS.fill({}, 0, 16)
    self.bg_pixels_idx = 0
    self.bg_pattern_base = 0    -- == 0 or 0x1000
    self.bg_pattern_base_15 = 0 -- == self.bg_pattern_base[12] << 15
    self.bg_pattern = 0
    self.bg_pattern_lut = TILE_LUT[1]
    self.bg_pattern_lut_fetched = TILE_LUT[1]
    -- invariant:
    --   self.bg_pattern_lut_fetched == TILE_LUT[
    --     self.nmt_ref[self.io_addr >> 10 & 3][self.io_addr & 0x03ff] >>
    --       ((self.scroll_addr_0_4 & 0x2) | (self.scroll_addr_5_14[6] * 0x4)) & 3
    --   ]

    ------ OAM-sprite state
    self.sp_enabled = false
    self.sp_active = false -- == self.sp_visible and self.sp_enabled
    self.batch_render_eight_pixels = self.batch_render_eight_pixels_bg
    self.sp_show = false
    self.sp_show_edge = false

    -- for CPU-PPU interface
    self.sp_base = 0
    self.sp_height = 8

    -- for OAM fetcher
    self.evaluate_sprites_odd_phase = self.evaluate_sprites_odd_phase_0
    self.sp_ram = UTILS.fill({}, 0xff, 0x100) -- ram size is 0x100, 0xff is a OAM garbage
    self.sp_index = 0
    self.sp_addr = 0
    self.sp_latch = 0

    -- for internal state
    -- 8 sprites per line are allowed in standard NES, but a user may remove this limit.
    self.sp_limit = (self.conf.sprite_limit and 8 or 32) * 4
    self.sp_buffer = UTILS.fill({}, 0, self.sp_limit)
    self.sp_buffered = 0
    self.sp_visible = false
    self.sp_map = {} -- [[behind?, zero?, color]]
    self.sp_map_buffer =
        map(
            range(0, 264),
            function()
                return { false, false, 0 }
            end
        ) -- preallocation for self.sp_map
    self.sp_zero_in_line = false
end

function PPU:update_output_color()
    for i = 1, 0x20 do
        local idx = bor(band(self.palette_ram[i], self.coloring), self.emphasis)
        self.output_color[i] = self.palette[idx]
    end
end

function PPU:setup_lut()
    --UTILS.print("setup_lut start")
    if self.lut_update == nil then
        self.lut_update = {}
    end

    self.name_lut =
        map(
            range(0, 0xffff),
            function(i, name_lut)
                local nmt_bank = self.nmt_ref[band(rshift(i, 10), 3)]
                local nmt_idx = band(i, 0x03ff)
                local fixed = bor(band(rshift(i, 12), 7), lshift(nthBitIsSetInt(i, 15), 12))
                if not self.lut_update[nmt_bank] then
                    self.lut_update[nmt_bank] = {}
                end
                if not self.lut_update[nmt_bank][nmt_idx] then
                    self.lut_update[nmt_bank][nmt_idx] = { nil, nil }
                end
                local upd = self.lut_update[nmt_bank][nmt_idx]
                if not upd[1] then
                    upd[1] = {}
                end
                upd = upd[1]
                upd[#upd + 1] = { i, fixed, name_lut }
                return bor(lshift(nmt_bank[nmt_idx], 4), fixed)
            end
        )

    --UTILS.print("setup_lut mid")
    local entries = {}
    self.attr_lut =
        map(
            range(0, 0x7fff),
            function(i)
                local io_addr = bor(0x23c0, band(i, 0x0c00), band(rshift(i, 4), 0x0038), band(rshift(i, 2), 0x0007))
                local nmt_bank = self.nmt_ref[band(rshift(io_addr, 10), 3)]
                local nmt_idx = band(io_addr, 0x03ff)
                local attr_shift = bor(band(i, 2), band(rshift(i, 4), 4))
                local key = tostring(io_addr) .. "-" .. tostring(attr_shift)
                if not entries[key] then
                    entries[key] = {
                        io_addr,
                        TILE_LUT[1 + band(rshift(nmt_bank[nmt_idx], attr_shift), 3)],
                        attr_shift
                    }
                end
                if not self.lut_update[nmt_bank] then
                    self.lut_update[nmt_bank] = {}
                end
                if not self.lut_update[nmt_bank][nmt_idx] then
                    self.lut_update[nmt_bank][nmt_idx] = { nil, nil }
                end
                local upd = self.lut_update[nmt_bank][nmt_idx]
                if not upd[2] then
                    upd[2] = {}
                end
                upd = upd[2]
                upd[#upd + 1] = entries[key]
                return entries[key]
            end
        )
    --UTILS.print("setup_lut end")
    return entries
end

--------------------------------------------------------------------------------------------------------------------
-- other APIs

function PPU:set_chr_mem(mem, writable)
    self.chr_mem = mem
    self.chr_mem_writable = writable
end

PPU.NMT_TABLE = {
    horizontal = { 0, 0, 1, 1 },
    vertical = { 0, 1, 0, 1 },
    four_screen = { 0, 1, 2, 3 },
    first = { 0, 0, 0, 0 },
    second = { 1, 1, 1, 1 }
}
function PPU:nametables(mode)
    self:update(RP2C02_CC)
    local idxs = PPU.NMT_TABLE[mode] or mode

    -- disable nametable LUT caching:
    --[[
    self.nmt_ref[0] = self.nmt_mem[idxs[1] ]
    self.nmt_ref[1] = self.nmt_mem[idxs[2] ]
    self.nmt_ref[2] = self.nmt_mem[idxs[3] ]
    self.nmt_ref[3] = self.nmt_mem[idxs[4]  ]

    self.lut_update = nil
    self.nt_cache.lut_update = nil
    self:setup_lut()
    if true then
        return
    end
    --]]
    if
        self.nmt_ref[0] == self.nmt_mem[idxs[1]] and
        self.nmt_ref[1] == self.nmt_mem[idxs[2]] and
        self.nmt_ref[2] == self.nmt_mem[idxs[3]] and
        self.nmt_ref[3] == self.nmt_mem[idxs[4]]
    then
        return
    end
    if
        self.nt_cache.lut_update ~= nil and
        self.nt_cache.nmt_ref[0] == self.nmt_mem[idxs[1]] and
        self.nt_cache.nmt_ref[1] == self.nmt_mem[idxs[2]] and
        self.nt_cache.nmt_ref[2] == self.nmt_mem[idxs[3]] and
        self.nt_cache.nmt_ref[3] == self.nmt_mem[idxs[4]]
    then
        self.nt_cache.nmt_ref[0] = self.nmt_ref[0]
        self.nt_cache.nmt_ref[1] = self.nmt_ref[1]
        self.nt_cache.nmt_ref[2] = self.nmt_ref[2]
        self.nt_cache.nmt_ref[3] = self.nmt_ref[3]

        local lut_update = self.nt_cache.lut_update
        local attr_lut = self.nt_cache.attr_lut
        local name_lut = self.nt_cache.name_lut
        self.nt_cache.lut_update = self.lut_update
        self.nt_cache.attr_lut = self.attr_lut
        self.nt_cache.name_lut = self.name_lut

        self.nmt_ref[0] = self.nmt_mem[idxs[1]]
        self.nmt_ref[1] = self.nmt_mem[idxs[2]]
        self.nmt_ref[2] = self.nmt_mem[idxs[3]]
        self.nmt_ref[3] = self.nmt_mem[idxs[4]]
        self.lut_update = lut_update
        self.attr_lut = attr_lut
        self.name_lut = name_lut

        return
    end
    if self.nt_cache.lut_update == nil then
        self.nt_cache.nmt_ref[0] = self.nmt_ref[0]
        self.nt_cache.nmt_ref[1] = self.nmt_ref[1]
        self.nt_cache.nmt_ref[2] = self.nmt_ref[2]
        self.nt_cache.nmt_ref[3] = self.nmt_ref[3]
        self.nt_cache.lut_update = self.lut_update
        self.nt_cache.attr_lut = self.attr_lut
        self.nt_cache.name_lut = self.name_lut
    else
        self.lut_update = nil
        self.nt_cache.lut_update = nil
    end

    self.nmt_ref[0] = self.nmt_mem[idxs[1]]
    self.nmt_ref[1] = self.nmt_mem[idxs[2]]
    self.nmt_ref[2] = self.nmt_mem[idxs[3]]
    self.nmt_ref[3] = self.nmt_mem[idxs[4]]

    self:setup_lut()
end

function PPU:update(data_setup)
    --print "ppu update"
    --print(debug.traceback())
    local ticks = (self.cpu):update()
    --print(data_setup)
    --print(ticks)
    --print(self.hclk_target)
    return self:sync(ticks + data_setup)
end

function PPU:setup_frame()
    --prof.push("setup_frame")
    local clr = self.palette[16]
    local output = self.output_pixels
    for i = 1, PPU.SCREEN_HEIGHT * PPU.SCREEN_WIDTH do
        output[i] = clr
    end
    self.output_pixels_size = 0
    self.odd_frame = not self.odd_frame
    local t = self.hclk == HCLOCK_DUMMY and DUMMY_FRAME or BOOT_FRAME
    self.vclk, self.hclk_target = t[1], t[2]
    self.cpu:set_next_frame_clock(t[3])
    --prof.pop("setup_frame")
end

function PPU:vsync()
    if self.hclk_target ~= FOREVER_CLOCK then
        self.hclk_target = FOREVER_CLOCK
        self:run()
    end
end

function PPU:monitor_a12_rising_edge(monitor)
    self.a12_monitor = monitor
    self.update_address_line = self.do_update_address_line
    self.update_scroll_address_line = update_scroll_address_line_monitor_a12
end

--------------------------------------------------------------------------------------------------------------------
-- helpers

function PPU:update_vram_addr()
    if self.vram_addr_inc == 32 then
        if self:isactive() then
            if band(self.scroll_addr_5_14, 0x7000) == 0x7000 then
                self.scroll_addr_5_14 = band(self.scroll_addr_5_14, 0x0fff)
                local x = band(self.scroll_addr_5_14, 0x03e0)
                if x == 0x03a0 then
                    self.scroll_addr_5_14 = bxor(self.scroll_addr_5_14, 0x0800)
                elseif x == 0x03e0 then
                    self.scroll_addr_5_14 = band(self.scroll_addr_5_14, 0x7c00)
                else
                    self.scroll_addr_5_14 = self.scroll_addr_5_14 + 0x20
                end
            else
                self.scroll_addr_5_14 = self.scroll_addr_5_14 + 0x1000
            end
        else
            self.scroll_addr_5_14 = self.scroll_addr_5_14 + 0x20
        end
    elseif self.scroll_addr_0_4 < 0x1f then
        self.scroll_addr_0_4 = self.scroll_addr_0_4 + 1
    else
        self.scroll_addr_0_4 = 0
        self.scroll_addr_5_14 = self.scroll_addr_5_14 + 0x20
    end
    return self:update_scroll_address_line()
end

function PPU:update_scroll_address_line()
    self.name_io_addr = bor(band(bor(self.scroll_addr_0_4, self.scroll_addr_5_14), 0x0fff), 0x2000)
end

function PPU:update_scroll_address_line_monitor_a12()
    self.name_io_addr = bor(band(bor(self.scroll_addr_0_4, self.scroll_addr_5_14), 0x0fff), 0x2000)
    if self.a12_monitor then
        local a12_state = band(self.scroll_addr_5_14, 0x3000) == 0x1000
        if not self.a12_state and a12_state then
            self.a12_monitor.a12_signaled(self.cpu:current_clock())
        end
        self.a12_state = a12_state
    end
end

function PPU:isactive()
    return self.scanline ~= SCANLINE_VBLANK and self.any_show
end

function PPU:sync(elapsed)
    if not (self.hclk_target < elapsed) then
        return
    end
    self.hclk_target = elapsed / RP2C02_CC - self.vclk
    return self:run()
end

function PPU:make_sure_invariants()
    local scroll_addr_5_14 = self.scroll_addr_5_14
    self.name_io_addr = bor(band(bor(self.scroll_addr_0_4, scroll_addr_5_14), 0x0fff), 0x2000)
    local a = self.nmt_ref[band(rshift(self.io_addr, 10), 3)][band(self.io_addr, 0x03ff)]
    local b = bor(band(self.scroll_addr_0_4, 0x2), (nthBitIsSetInt(scroll_addr_5_14, 6) * 0x4))
    local idx = 1 + band(rshift(a, b), 3)
    self.bg_pattern_lut_fetched = TILE_LUT[idx]
end

function PPU:io_latch_mask(data)
    if self:isactive() then
        return 0xff
    elseif band(self.regs_oam, 0x03) == 0x02 then
        return band(data, 0xe3)
    else
        return data
    end
end

--------------------------------------------------------------------------------------------------------------------
-- mapped memory handlers

-- PPUCTRL
function PPU:poke_2000(_addr, data)
    self:update(RP2C02_CC)
    local need_nmi_old = self.need_nmi

    self.scroll_latch = bor(band(self.scroll_latch, 0x73ff), lshift(band(data, 0x03), 10))
    self.vram_addr_inc = nthBitIsSetInt(data, 2) == 1 and 32 or 1
    self.sp_base = nthBitIsSetInt(data, 3) == 1 and 0x1000 or 0x0000
    self.bg_pattern_base = (nthBitIsSetInt(data, 4) == 1) and 0x1000 or 0x0000
    self.sp_height = nthBitIsSetInt(data, 5) == 1 and 16 or 8
    self.need_nmi = nthBitIsSetInt(data, 7) == 1

    self.io_latch = data
    self.pattern_end = (self.sp_base ~= 0 or self.sp_height == 16) and 0x1ff0 or 0x0ff0
    self.bg_pattern_base_15 = lshift(nthBitIsSetInt(self.bg_pattern_base, 12), 15)

    if self.need_nmi and self.vblank and not need_nmi_old then
        local clock = self.cpu:current_clock() + RP2C02_CC
        if clock < PPU.RP2C02_HVINT then
            self.cpu:do_nmi(clock)
        end
    end
end

-- PPUMASK
function PPU:poke_2001(_addr, data)
    self:update(RP2C02_CC)
    local bg_show_old, bg_show_edge_old = self.bg_show, self.bg_show_edge
    local sp_show_old, sp_show_edge_old = self.sp_show, self.sp_show_edge
    local any_show_old = self.any_show
    local coloring_old, emphasis_old = self.coloring, self.emphasis

    self.bg_show = nthBitIsSetInt(data, 3) == 1
    self.bg_show_edge = nthBitIsSetInt(data, 1) == 1 and self.bg_show
    self.sp_show = nthBitIsSetInt(data, 4) == 1
    self.sp_show_edge = nthBitIsSetInt(data, 2) == 1 and self.sp_show
    self.any_show = self.bg_show or self.sp_show
    self.coloring = nthBitIsSetInt(data, 0) == 1 and 0x30 or 0x3f -- 0x30: monochrome
    self.emphasis = lshift(band(data, 0xe0), 1)

    self.io_latch = data

    if
        bg_show_old ~= self.bg_show or bg_show_edge_old ~= self.bg_show_edge or sp_show_old ~= self.sp_show or
        sp_show_edge_old ~= self.sp_show_edge
    then
        if self.hclk < 8 or self.hclk >= 248 then
            self:update_enabled_flags_edge()
        else
            self:update_enabled_flags()
        end
        if any_show_old and not self.any_show then
            self:update_scroll_address_line()
        end
    end

    if coloring_old ~= self.coloring or emphasis_old ~= self.emphasis then
        self:update_output_color()
    end
end

-- PPUSTATUS
function PPU:peek_2002(_addr)
    self:update(RP2C02_CC)
    local v = band(self.io_latch, 0x1f)
    if self.vblank then
        v = bor(v, 0x80)
    end
    if self.sp_zero_hit then
        v = bor(v, 0x40)
    end
    if self.sp_overflow then
        v = bor(v, 0x20)
    end
    self.io_latch = v
    self.scroll_toggle = false
    self.vblanking = false
    self.vblank = false
    return self.io_latch
end

-- OAMADDR
function PPU:poke_2003(_addr, data)
    self:update(RP2C02_CC)
    self.regs_oam = data
    self.io_latch = data
end

-- OAMDATA (write)
function PPU:poke_2004(_addr, data)
    self:update(RP2C02_CC)
    self.sp_ram[self.regs_oam + 1] = self:io_latch_mask(data)
    self.io_latch = self.sp_ram[self.regs_oam + 1]
    self.regs_oam = band(self.regs_oam + 1, 0xff)
end

-- OAMDATA (read)
function PPU:peek_2004(_addr)
    if
        not self.any_show or
        self.cpu:current_clock() - (self.cpu:next_frame_clock() - (341 * 241) * RP2C02_CC) >=
        (341 * 240) * RP2C02_CC
    then
        self.io_latch = self.sp_ram[self.regs_oam + 1]
    else
        self:update(RP2C02_CC)
        self.io_latch = self.sp_latch
    end
end

-- PPUSCROLL
function PPU:poke_2005(_addr, data)
    self:update(RP2C02_CC)
    self.io_latch = data
    self.scroll_toggle = not self.scroll_toggle
    if self.scroll_toggle then
        self.scroll_latch = bor(band(self.scroll_latch, 0x7fe0), rshift(data, 3))
        local xfine = 8 - band(data, 0x7)
        self.bg_pixels_idx = rotatePositiveIdx(self.bg_pixels, self.bg_pixels_idx + self.scroll_xfine - xfine)
        self.scroll_xfine = xfine
    else
        self.scroll_latch = bor(band(self.scroll_latch, 0x0c1f), band(bor(lshift(data, 2), lshift(data, 12)), 0x73e0))
    end
end

-- PPUADDR
function PPU:poke_2006(_addr, data)
    self:update(RP2C02_CC)
    self.io_latch = data
    self.scroll_toggle = not self.scroll_toggle
    if self.scroll_toggle then
        self.scroll_latch = bor(band(self.scroll_latch, 0x00ff), lshift(band(data, 0x3f), 8))
    else
        self.scroll_latch = bor(band(self.scroll_latch, 0x7f00), data)
        self.scroll_addr_0_4 = band(self.scroll_latch, 0x001f)
        self.scroll_addr_5_14 = band(self.scroll_latch, 0x7fe0)
        self:update_scroll_address_line()
    end
end

-- PPUDATA (write)
function PPU:poke_2007(_addr, data)
    self:update(RP2C02_CC * 4)
    local addr = bor(self.scroll_addr_0_4, self.scroll_addr_5_14)
    self:update_vram_addr()
    self.io_latch = data
    if band(addr, 0x3f00) == 0x3f00 then
        addr = band(addr, 0x1f)
        local final = self.palette[1 + bor(band(data, self.coloring), self.emphasis)]
        self.palette_ram[addr + 1] = data
        self.output_color[addr + 1] = final
        if band(addr, 3) == 0 then
            local addr = bxor(addr, 0x10) + 1
            self.palette_ram[addr] = data
            self.output_color[addr] = final
        end
        self.output_bg_color = band(self.palette_ram[1], 0x3f)
    else
        addr = band(addr, 0x3fff)
        if addr >= 0x2000 then
            -- nametable RAM
            local nmt_bank = self.nmt_ref[band(rshift(addr, 10), 0x3)]
            local nmt_idx = band(addr, 0x03ff)
            if nmt_bank[nmt_idx] ~= data then
                nmt_bank[nmt_idx] = data
                local t = self.lut_update[nmt_bank][nmt_idx]
                local name_lut_update, attr_lut_update = t[1], t[2]
                if name_lut_update then
                    for i = 1, #name_lut_update do
                        local t = name_lut_update[i]
                        local name_lut_to_update = t[3] -- self.name_lut
                        name_lut_to_update[t[1]] = bor(lshift(data, 4), t[2])
                    end
                end
                if attr_lut_update then
                    for i = 1, #attr_lut_update do
                        local a = attr_lut_update[i]
                        a[2] = TILE_LUT[1 + band(rshift(data, a[3]), 3)]
                    end
                end
            end
        elseif self.chr_mem_writable then
            self.chr_poke(addr, data)
            --self.chr_mem[addr + 1] = data
        end
    end
end

-- PPUDATA (read)
function PPU:peek_2007(_addr)
    self:update(RP2C02_CC)
    local addr = band(bor(self.scroll_addr_0_4, self.scroll_addr_5_14), 0x3fff)
    self:update_vram_addr()
    self.io_latch =
        band(addr, 0x3f00) ~= 0x3f00 and self.io_buffer or band(self.palette_ram[band(addr, 0x1f) + 1], self.coloring)
    self.io_buffer =
        addr >= 0x2000 and self.nmt_ref[band(rshift(addr, 10), 0x3)][band(addr, 0x3ff)] or
        self.chr_peek(addr) --self.chr_mem[addr + 1]
    return self.io_latch
end

function PPU:poke_2xxx(_addr, data)
    self.io_latch = data
end

function PPU:peek_2xxx(_addr)
    return self.io_latch
end

function PPU:peek_3000(_addr)
    self:update(RP2C02_CC)
    return self.io_latch
end

-- OAMDMA
function PPU:poke_4014(_addr, data) -- DMA
    if self.cpu:odd_clock() then
        self.cpu:steal_clocks(CPU.CLK[1])
    end
    self:update(RP2C02_CC)
    self.cpu:steal_clocks(CPU.CLK[1])
    data = lshift(data, 8)
    if
        self.regs_oam == 0 and data < 0x2000 and
        ((not self.any_show) or self.cpu:current_clock() <= PPU.RP2C02_HVINT - CPU.CLK[1] * 512)
    then
        self.cpu:steal_clocks(CPU.CLK[1] * 512)
        self.cpu:sprite_dma(band(data, 0x7ff), self.sp_ram)
        self.io_latch = self.sp_ram[0xff + 1]
    else
        repeat
            self.io_latch = self.cpu:fetch(data)
            data = data + 1
            self.cpu:steal_clocks(CPU.CLK[1])
            self:update(RP2C02_CC)
            self.cpu:steal_clocks(CPU.CLK[1])
            self.io_latch = self:io_latch_mask(self.io_latch)
            self.sp_ram[self.regs_oam + 1] = self.io_latch
            self.regs_oam = band(self.regs_oam + 1, 0xff)
        until not (band(data, 0xff) ~= 0)
    end
end

function PPU:peek_4014(_addr)
    return 0x40
end

------------------------------------------------------------------------------------------------------------------------------------------------------
-- helper methods for PPU--run

function PPU:open_pattern(exp)
    if not self.any_show then
        return
    end
    self.io_addr = exp
    local ual = self:update_address_line()
    return ual
end

function PPU:open_sprite(buffer_idx)
    local sp_buffer = self.sp_buffer
    local flip_v = nthBitIsSetInt(sp_buffer[buffer_idx + 3], 7) -- OAM byte2 bit7: "Flip vertically" flag
    local tmp = bxor((self.scanline - sp_buffer[buffer_idx + 1]), (flip_v * 0xf))
    local byte1 = sp_buffer[buffer_idx + 2]
    local addr =
        self.sp_height == 16 and
        bor(lshift(band(byte1, 0x01), 12), lshift(band(byte1, 0xfe), 4), (nthBitIsSetInt(tmp, 3) * 0x10)) or
        bor(self.sp_base, lshift(byte1, 4))
    local ret = bor(addr, band(tmp, 7))
    return ret
end

function PPU:load_sprite(pat0, pat1, buffer_idx)
    local sp_buffer = self.sp_buffer
    local byte2 = sp_buffer[buffer_idx + 3]
    local pos = PPU.SP_PIXEL_POSITIONS[1 + nthBitIsSetInt(byte2, 6)] -- OAM byte2 bit6: "Flip horizontally" flag
    local pat =
        bor(
            band(rshift(pat0, 1), 0x55),
            band(pat1, 0xaa),
            lshift(bor(band(pat0, 0x55), band(lshift(pat1, 1), 0xaa)), 8)
        )
    local x_base = sp_buffer[buffer_idx + 4]
    local palette_base = 0x10 + lshift(band(byte2, 3), 2) -- OAM byte2 bit0-1: Palette
    if not self.sp_visible then
        self.sp_visible = true
        self.sp_map = {}
        --table.clear(self.sp_map)
    end
    for i = 1, 8 do
        local x = x_base + i
        local clr = band(rshift(pat, (pos[i] * 2)), 3)
        if not (self.sp_map[x] or clr == 0) then
            local sprite = self.sp_map_buffer[x]
            self.sp_map[x] = sprite
            -- sprite[0]: behind flag, sprite[1]: zero hit flag, sprite[2]: color
            sprite[0] = nthBitIsSetInt(byte2, 5) == 1 -- OAM byte2 bit5: "Behind background" flag
            sprite[1] = buffer_idx == 0 and self.sp_zero_in_line
            sprite[2] = palette_base + clr
        end
    end
    local sp_enabled = self.sp_enabled
    self.sp_active = sp_enabled
    self.batch_render_eight_pixels =
        sp_enabled and self.batch_render_eight_pixels_sp or self.batch_render_eight_pixels_bg
end

function PPU:update_address_line()
end

function PPU:do_update_address_line()
    if self.a12_monitor then
        local a12_state = band(self.io_addr, 0x1000) == 0x1000
        if not self.a12_state and a12_state then
            self.a12_monitor:a12_signaled((self.vclk + self.hclk) * RP2C02_CC)
        end
        self.a12_state = a12_state
    end
end

------------------------------------------------------------------------------------------------------------------------------------------------------
-- actions for PPU--run

function PPU:open_name()
    if not self.any_show then
        return
    end
    self.io_addr = self.name_io_addr
    return self:update_address_line()
end

function PPU:fetch_name()
    if not self.any_show then
        return
    end
    self.io_pattern = self.name_lut[self.scroll_addr_0_4 + self.scroll_addr_5_14 + self.bg_pattern_base_15]
end

function PPU:open_attr()
    if not self.any_show then
        return
    end
    local t = self.attr_lut[bor(self.scroll_addr_0_4, self.scroll_addr_5_14)]
    self.io_addr, self.bg_pattern_lut_fetched = t[1], t[2]
    return self:update_address_line()
end

function PPU:fetch_attr()
    if not self.any_show then
        return
    end
    self.bg_pattern_lut = self.bg_pattern_lut_fetched
    -- raise unless self.bg_pattern_lut_fetched ==
    --   self.nmt_ref[self.io_addr >> 10 & 3][self.io_addr & 0x03ff] >>
    --     ((self.scroll_addr_0_4 & 0x2) | (self.scroll_addr_5_14[6] * 0x4)) & 3
end

function PPU:fetch_bg_pattern_0()
    if not self.any_show then
        return
    end
    --UTILS.print(string.format("fetch_bg_pattern_0 addr:%04X hclk:%d", band(self.io_addr, 0x1fff), self.hclk))
    self.bg_pattern = self.chr_bg_read(band(self.io_addr, 0x1fff))
    --self.bg_pattern = self.chr_mem[1 + band(self.io_addr, 0x1fff)]
end

function PPU:fetch_bg_pattern_1()
    if not self.any_show then
        return
    end
    --UTILS.print(string.format("fetch_bg_pattern_1 addr:%04X hclk:%d", band(self.io_addr, 0x1fff), self.hclk))
    self.bg_pattern = bor(self.bg_pattern, self.chr_bg_read(band(self.io_addr, 0x1fff)) * 0x100)
    --self.bg_pattern = bor(self.bg_pattern, self.chr_mem[1 + band(self.io_addr, 0x1fff)] * 0x100)
end

function PPU:scroll_clock_x()
    if not self.any_show then
        return
    end
    if self.scroll_addr_0_4 < 0x001f then
        self.scroll_addr_0_4 = self.scroll_addr_0_4 + 1
        self.name_io_addr = self.name_io_addr + 1 -- make cache consistent
    else
        self.scroll_addr_0_4 = 0
        self.scroll_addr_5_14 = bxor(self.scroll_addr_5_14, 0x0400)
        self.name_io_addr = bxor(self.name_io_addr, 0x041f) -- make cache consistent
    end
end

function PPU:scroll_reset_x()
    if not self.any_show then
        return
    end
    self.scroll_addr_0_4 = band(self.scroll_latch, 0x001f)
    self.scroll_addr_5_14 = bor(band(self.scroll_addr_5_14, 0x7be0), band(self.scroll_latch, 0x0400))
    self.name_io_addr = bor(band(bor(self.scroll_addr_0_4, self.scroll_addr_5_14), 0x0fff), 0x2000) -- make cache consistent
end

function PPU:scroll_clock_y()
    if not self.any_show then
        return
    end
    if band(self.scroll_addr_5_14, 0x7000) ~= 0x7000 then
        self.scroll_addr_5_14 = self.scroll_addr_5_14 + 0x1000
    else
        local mask = band(self.scroll_addr_5_14, 0x03e0)
        if mask == 0x03a0 then
            self.scroll_addr_5_14 = bxor(self.scroll_addr_5_14, 0x0800)
            self.scroll_addr_5_14 = band(self.scroll_addr_5_14, 0x0c00)
        elseif mask == 0x03e0 then
            self.scroll_addr_5_14 = band(self.scroll_addr_5_14, 0x0c00)
        else
            self.scroll_addr_5_14 = band(self.scroll_addr_5_14, 0x0fe0) + 32
        end
    end

    self.name_io_addr = bor(band(bor(self.scroll_addr_0_4, self.scroll_addr_5_14), 0x0fff), 0x2000) -- make cache consistent
end

function PPU:preload_tiles()
    if not self.any_show then
        return
    end
    local patt = self.bg_pattern_lut[self.bg_pattern]
    local length = #self.bg_pixels
    local idx = self.scroll_xfine + 1
    for i = 0, 7 do --8 do
        self:set_bg_pxs(i + idx, patt[i], length)
    end
end

function PPU:index_bg_pxs(idx, size)
    return self.bg_pixels[rotatePositiveIdx(self.bg_pixels, self.bg_pixels_idx + idx, size)]
end

function PPU:set_bg_pxs(idx, v, size)
    self.bg_pixels[rotatePositiveIdx(self.bg_pixels, self.bg_pixels_idx + idx, size)] = v
end

function PPU:load_tiles()
    local length = #self.bg_pixels
    self.bg_pixels_idx = rotatePositiveIdx(self.bg_pixels, self.bg_pixels_idx + 8, length)
    local patt = self.bg_pattern_lut[self.bg_pattern]
    local idx = self.scroll_xfine + 1
    for i = 0, 7 do --8 do
        self:set_bg_pxs(i + idx, patt[i], length)
    end
end

function PPU:evaluate_sprites_even()
    self.sp_latch = self.sp_ram[self.sp_addr + 1]
end

function PPU:evaluate_sprites_odd()
    if not self.any_show then
        return
    end
    self:evaluate_sprites_odd_phase()
end

function PPU:evaluate_sprites_odd_phase_0()
end

function PPU:evaluate_sprites_odd_phase_1()
    self.sp_index = self.sp_index + 1
    if self.sp_latch <= self.scanline and self.scanline < self.sp_latch + self.sp_height then
        --print "phase 2"
        self.sp_addr = self.sp_addr + 1
        self.evaluate_sprites_odd_phase = self.evaluate_sprites_odd_phase_2
        self.sp_buffer[self.sp_buffered + 1] = self.sp_latch
    elseif self.sp_index == 64 then
        self.sp_addr = 0
        self.evaluate_sprites_odd_phase = self.evaluate_sprites_odd_phase_9
    elseif self.sp_index == 2 then
        self.sp_addr = 8
    else
        self.sp_addr = self.sp_addr + 4
    end
end

function PPU:evaluate_sprites_odd_phase_2()
    self.sp_addr = self.sp_addr + 1
    self.evaluate_sprites_odd_phase = self.evaluate_sprites_odd_phase_3
    self.sp_buffer[self.sp_buffered + 2] = self.sp_latch
end

function PPU:evaluate_sprites_odd_phase_3()
    self.sp_addr = self.sp_addr + 1
    self.evaluate_sprites_odd_phase = self.evaluate_sprites_odd_phase_4
    self.sp_buffer[self.sp_buffered + 3] = self.sp_latch
end

function PPU:evaluate_sprites_odd_phase_4()
    self.sp_buffer[self.sp_buffered + 4] = self.sp_latch
    self.sp_buffered = self.sp_buffered + 4
    --print "evaluate_sprites_odd_phase_4"
    --print(self.sp_buffered)
    if self.sp_index ~= 64 then
        if self.sp_buffered ~= self.sp_limit then
            self.evaluate_sprites_odd_phase = self.evaluate_sprites_odd_phase_1
        else
            self.evaluate_sprites_odd_phase = self.evaluate_sprites_odd_phase_5
        end
        if self.sp_index ~= 2 then
            self.sp_addr = self.sp_addr + 1
            if not self.sp_zero_in_line then
                self.sp_zero_in_line = self.sp_index == 1
            end
        else
            self.sp_addr = 8
        end
    else
        self.sp_addr = 0
        self.evaluate_sprites_odd_phase = self.evaluate_sprites_odd_phase_9
    end
end

function PPU:evaluate_sprites_odd_phase_5()
    if self.sp_latch <= self.scanline and self.scanline < self.sp_latch + self.sp_height then
        self.evaluate_sprites_odd_phase = self.evaluate_sprites_odd_phase_6
        self.sp_addr = band(self.sp_addr + 1, 0xff)
        self.sp_overflow = true
    else
        self.sp_addr = band((self.sp_addr + 4), 0xfc) + band((self.sp_addr + 1), 3)
        if self.sp_addr <= 5 then
            self.evaluate_sprites_odd_phase = self.evaluate_sprites_odd_phase_9
            self.sp_addr = band(self.sp_addr, 0xfc)
        end
    end
end

function PPU:evaluate_sprites_odd_phase_6()
    self.evaluate_sprites_odd_phase = self.evaluate_sprites_odd_phase_7
    self.sp_addr = band(self.sp_addr + 1, 0xff)
end

function PPU:evaluate_sprites_odd_phase_7()
    self.evaluate_sprites_odd_phase = self.evaluate_sprites_odd_phase_8
    self.sp_addr = band(self.sp_addr + 1, 0xff)
end

function PPU:evaluate_sprites_odd_phase_8()
    self.evaluate_sprites_odd_phase = self.evaluate_sprites_odd_phase_9
    self.sp_addr = band(self.sp_addr + 1, 0xff)
    if band(self.sp_addr, 3) == 3 then
        self.sp_addr = self.sp_addr + 1
    end
    self.sp_addr = band(self.sp_addr, 0xfc)
end

function PPU:evaluate_sprites_odd_phase_9()
    self.sp_addr = band(self.sp_addr + 4, 0xff)
end

function PPU:load_extended_sprites()
    if not self.any_show then
        return
    end
    if 32 < self.sp_buffered then
        local buffer_idx = 32
        repeat
            local addr = self:open_sprite(buffer_idx)

            local pat0 = self.chr_sprite_read(addr)
            local pat1 = self.chr_sprite_read(bor(addr, 8))
            --local pat0 = self.chr_mem[1 + addr]
            --local pat1 = self.chr_mem[1 + bor(addr, 8)]
            if pat0 ~= 0 or pat1 ~= 0 then
                self:load_sprite(pat0, pat1, buffer_idx)
            end
            buffer_idx = buffer_idx + 4
        until not (buffer_idx ~= self.sp_buffered)
    end
end

function PPU:render_pixel()
    self:set_bg_pxs(self.hclk % 8 + 1, 0)
    local px = self.output_color[band(self.scroll_addr_5_14, 0x3f00) == 0x3f00 and self.scroll_addr_0_4 or 0]
    if px then
        self.output_pixels_size = self.output_pixels_size + 1
        self.output_pixels[self.output_pixels_size] = px
    end
end

function PPU:batch_render_eight_pixels_sp()
    local pixel
    local output_color = self.output_color
    local clr = output_color[1]
    if self.bg_enabled then
        local hclk = self.hclk
        local hclk255 = hclk ~= 255
        local sp_map = self.sp_map
        local size = self.output_pixels_size
        local output = self.output_pixels
        for i = 1, 8 do
            local px = self:index_bg_pxs(i)
            local sprite = sp_map[hclk + i - 1]
            if sprite then
                if px % 4 == 0 then
                    px = sprite[2]
                else
                    if sprite[1] and hclk255 then
                        self.sp_zero_hit = true
                    end
                    if not sprite[0] then
                        px = sprite[2]
                    end
                end
            end
            output[size + i] = output_color[1 + px] or clr
        end
    else
        for i = 1, 8 do
            local sprite = self.sp_map[self.hclk + i - 1]
            self.output_pixels[self.output_pixels_size + i] = output_color[sprite and (sprite[2] + 1) or 1] or clr
        end
    end
    self.output_pixels_size = self.output_pixels_size + 8
end

function PPU:batch_render_eight_pixels_bg()
    local pixel
    local output_color = self.output_color
    local clr = output_color[1]
    if self.bg_enabled then
        for i = 1, 8 do
            local v = self:index_bg_pxs((self.hclk % 8) + i)
            self.output_pixels[self.output_pixels_size + i] = output_color[1 + v] or clr
        end
    else
        for i = 1, 8 do
            self.output_pixels[self.output_pixels_size + i] = clr
        end
    end
    self.output_pixels_size = self.output_pixels_size + 8
end

function PPU:boot()
    self.vblank = true
    self.hclk = HCLOCK_DUMMY
    self.hclk_target = FOREVER_CLOCK
end

function PPU:vblank_0()
    self.vblanking = true
    self.hclk = PPU.HCLOCK_VBLANK_1
end

function PPU:vblank_1()
    if not self.vblank then
        self.vblank = self.vblanking
    end
    self.vblanking = false
    self.sp_visible = false
    self.sp_active = false
    self.batch_render_eight_pixels = self.batch_render_eight_pixels_bg
    self.hclk = PPU.HCLOCK_VBLANK_2
end

function PPU:vblank_2()
    if not self.vblank then
        self.vblank = self.vblanking
    end
    self.vblanking = false
    self.hclk = HCLOCK_DUMMY
    self.hclk_target = FOREVER_CLOCK
    if self.need_nmi and self.vblank then
        self.cpu:do_nmi(self.cpu:next_frame_clock())
    end
end

function PPU:update_enabled_flags()
    if not self.any_show then
        return
    end
    self.bg_enabled = self.bg_show
    self.sp_enabled = self.sp_show
    local sp_active = self.sp_enabled and self.sp_visible
    self.sp_active = sp_active
    self.batch_render_eight_pixels =
        sp_active and self.batch_render_eight_pixels_sp or self.batch_render_eight_pixels_bg
end

function PPU:update_enabled_flags_edge()
    self.bg_enabled = self.bg_show_edge
    self.sp_enabled = self.sp_show_edge
    local sp_active = self.sp_enabled and self.sp_visible
    self.sp_active = sp_active
    self.batch_render_eight_pixels =
        sp_active and self.batch_render_eight_pixels_sp or self.batch_render_eight_pixels_bg
end

------------------------------------------------------------------------------------------------------------------------------------------------------
-- default core

function PPU:debug_logging(scanline, hclk, hclk_target)
    if hclk == FOREVER_CLOCK then
        hclk = "forever"
    end
    if hclk_target == FOREVER_CLOCK then
        hclk_target = "forever"
    end

    print("ppu: scanline --{ scanline }, hclk --{ hclk }->--{ hclk_target }")
end

function PPU:run()
    --prof.push("fiber_if")
    if not self.fiber then
        --self:boot()
        self.bound_main_loop = UTILS.bind(self.orig_main_loop, self)
        self.fiber = coroutine.wrap(self.bound_main_loop)
        --prof.pop("fiber_if")
        --return
    end
    --prof.pop("fiber_if")

    --if self.conf.loglevel >= 3 then self:debug_logging(self.scanline, self.hclk, self.hclk_target) end

    --prof.push("make_sure_invariants")
    self:make_sure_invariants()
    --prof.pop("make_sure_invariants")

    --prof.push("fiber")
    local is_frame_wait = self.fiber()
    if not is_frame_wait then
        self.hclk_target = (self.vclk + self.hclk) * RP2C02_CC
        --else
        --prof.push("wrap")
        --self.fiber = coroutine.wrap(self.bound_main_loop)
        --prof.pop("wrap")
    end
    --prof.pop("fiber")

    if self.inform_scanline_counter then
        self.inform_scanline_counter = false
        self.scanline_counter_callback()
    end
end

function PPU:wait_frame()
    --print "wait ppu frame"
    coroutine.yield(true)
end

function PPU:wait_zero_clocks()
    if self.hclk_target <= self.hclk then
        coroutine.yield(false)
    end
end

function PPU:wait_one_clock()
    self.hclk = self.hclk + 1
    self:wait_zero_clocks()
end

function PPU:wait_two_clocks()
    self.hclk = self.hclk + 2
    self:wait_zero_clocks()
end

function PPU:pre_render()
    self.vblank = false
    self.sp_overflow = false
    self.sp_zero_hit = false
    self.vblanking = false
    self.scanline = PPU.SCANLINE_HDUMMY
    for i = 1, 32 do
        --341.step(589, 8) do
        -- when 341, 349, ..., 589
        self:open_name()
        self:wait_two_clocks()

        -- when 343, 351, ..., 591
        self:open_attr()
        self:wait_two_clocks()

        -- when 345, 353, ..., 593
        self:open_pattern(self.bg_pattern_base)
        self:wait_two_clocks()

        -- when 347, 355, ..., 595
        self:open_pattern(bor(self.io_addr, 8))
        self:wait_two_clocks()
    end

    for i = 1, 8 do
        --597.step(653, 8) do
        -- when 597, 605, ..., 653
        if self.any_show and self.hclk == 645 then
            self.scroll_addr_0_4 = band(self.scroll_latch, 0x001f)
            self.scroll_addr_5_14 = band(self.scroll_latch, 0x7fe0)
            self.name_io_addr = bor(band(bor(self.scroll_addr_0_4, self.scroll_addr_5_14), 0x0fff), 0x2000) -- make cache consistent
        end
        self:open_name()
        self:wait_two_clocks()

        -- when 599, 607, ..., 655
        -- Nestopia uses open_name here?
        self:open_attr()
        self:wait_two_clocks()

        -- when 601, 609, ..., 657
        self:open_pattern(self.pattern_end)
        self:wait_two_clocks()

        -- when 603, 611, ..., 659
        self:open_pattern(bor(self.io_addr, 8))
        if self.hclk == 659 then
            self.hclk = 320
            self.vclk = self.vclk + HCLOCK_DUMMY
            self.hclk_target = self.hclk_target - HCLOCK_DUMMY
        else
            self:wait_two_clocks()
        end
        self:wait_zero_clocks()
    end
end

function PPU:pre_render_scanline()
    -- when 320
    self:load_extended_sprites()
    self:open_name()
    if self.any_show then
        self.sp_latch = self.sp_ram[1]
    end
    self.sp_buffered = 0
    self.sp_zero_in_line = false
    self.sp_index = 0
    self.evaluate_sprites_odd_phase = self.evaluate_sprites_odd_phase_0
    self:wait_one_clock()

    -- when 321
    self:fetch_name()
    self:wait_one_clock()

    -- when 322
    self:open_attr()
    self:wait_one_clock()

    -- when 323
    self:fetch_attr()
    self:scroll_clock_x()
    self:wait_one_clock()

    -- when 324
    self:open_pattern(self.io_pattern)
    self:wait_one_clock()

    -- when 325
    self:fetch_bg_pattern_0()
    self:wait_one_clock()

    -- when 326
    self:open_pattern(bor(self.io_pattern, 8))
    self:wait_one_clock()

    -- when 327
    self:fetch_bg_pattern_1()
    self:wait_one_clock()

    -- when 328
    self:preload_tiles()
    self:open_name()
    self:wait_one_clock()

    -- when 329
    self:fetch_name()
    self:wait_one_clock()

    -- when 330
    self:open_attr()
    self:wait_one_clock()

    -- when 331
    self:fetch_attr()
    self:scroll_clock_x()
    self:wait_one_clock()

    -- when 332
    self:open_pattern(self.io_pattern)
    self:wait_one_clock()

    -- when 333
    self:fetch_bg_pattern_0()
    self:wait_one_clock()

    -- when 334
    self:open_pattern(bor(self.io_pattern, 8))
    self:wait_one_clock()

    -- when 335
    self:fetch_bg_pattern_1()
    self:wait_one_clock()

    -- when 336
    if self.any_show and self.scanline + 1 == self.scanline_counter_target and self.scanline_counter_target ~= 0 then
        self.inform_scanline_counter = true
        self.inform_scanline_counter_clk = true
    end
    self:open_name()
    self:wait_one_clock()

    -- when 337
    if self.any_show then
        self:update_enabled_flags_edge()
        if self.scanline == PPU.SCANLINE_HDUMMY and self.odd_frame then
            self.cpu:set_next_frame_clock(PPU.RP2C02_HVSYNC_1)
        end
    end
    self:wait_one_clock()

    -- when 338
    self:open_name()
    self.scanline = self.scanline + 1
end

function PPU:render_scanline()
    for i = 1, 32 do
        --0.step(248, 8) do
        -- when 0, 8, ..., 248
        if self.any_show then
            if self.hclk == 64 then
                --print "hclk 64 sp addr"
                self.sp_addr = band(self.regs_oam, 0xf8) -- SP_OFFSET_TO_0_1
                self.evaluate_sprites_odd_phase = self.evaluate_sprites_odd_phase_1
                self.sp_latch = 0xff
            end
            self:load_tiles()
            self:batch_render_eight_pixels()
            if self.hclk >= 64 then
                self:evaluate_sprites_even()
            end
            self:open_name()
        else
            self:render_pixel()
        end
        self:wait_one_clock()

        -- when 1, 9, ..., 249
        --print "when 1,9"
        --print(self.any_show)
        --print(self.hclk)
        if self.any_show then
            self:fetch_name()
            if self.hclk >= 64 then
                self:evaluate_sprites_odd()
            end
        else
            self:render_pixel()
        end
        self:wait_one_clock()

        -- when 2, 10, ..., 250
        if self.any_show then
            if self.hclk >= 64 then
                self:evaluate_sprites_even()
            end
            self:open_attr()
        else
            self:render_pixel()
        end
        self:wait_one_clock()

        -- when 3, 11, ..., 251
        if self.any_show then
            self:fetch_attr()
            if self.hclk >= 64 then
                self:evaluate_sprites_odd()
            end
            if self.hclk == 251 then
                self:scroll_clock_y()
            end
            self:scroll_clock_x()
        else
            self:render_pixel()
        end
        self:wait_one_clock()

        -- when 4, 12, ..., 252
        if self.any_show then
            if self.hclk >= 64 then
                self:evaluate_sprites_even()
            end
            self:open_pattern(self.io_pattern)
        else
            self:render_pixel()
        end
        self:wait_one_clock()

        if self.any_show then
            self:fetch_bg_pattern_0()
            if self.hclk >= 64 then
                self:evaluate_sprites_odd()
            end
        else
            self:render_pixel()
        end
        self:wait_one_clock()

        if self.any_show then
            if self.hclk >= 64 then
                self:evaluate_sprites_even()
            end
            self:open_pattern(bor(self.io_pattern, 8))
        else
            self:render_pixel()
        end
        self:wait_one_clock()

        if self.any_show then
            self:fetch_bg_pattern_1()
            if self.hclk >= 64 then
                self:evaluate_sprites_odd()
            end
        else
            self:render_pixel()
        end
        if self.hclk ~= 255 and self.any_show then
            self:update_enabled_flags()
        end
        self:wait_one_clock()
    end
end

function PPU:post_render_scanline()
    for i = 1, 8 do
        --256.step(312, 8) do
        if self.hclk == 256 then
            -- when 256
            self:open_name()
            if self.any_show then
                self.sp_latch = 0xff
            end
            self:wait_one_clock()
            -- when 257
            self:scroll_reset_x()
            self.sp_visible = false
            self.sp_active = false
            self.batch_render_eight_pixels = self.batch_render_eight_pixels_bg
            self:wait_one_clock()
        else
            -- when 264, 272, ..., 312
            self:open_name()
            self:wait_two_clocks()
        end

        -- when 258, 266, ..., 314
        -- Nestopia uses open_name here?
        self:open_attr()
        self:wait_two_clocks()

        -- when 260, 268, ..., 316
        if self.any_show then
            local buffer_idx = (self.hclk - 260) / 2
            self:open_pattern(buffer_idx >= self.sp_buffered and self.pattern_end or self:open_sprite(buffer_idx))
            if self.scanline == 238 and self.hclk == 316 then
                self.regs_oam = 0
            end
        end
        self:wait_one_clock()

        -- when 261, 269, ..., 317
        if self.any_show then
            if (self.hclk - 261) / 2 < self.sp_buffered then
                self.io_pattern = self.chr_sprite_read(band(self.io_addr, 0x1fff))
                --self.io_pattern = self.chr_mem[1 + band(self.io_addr, 0x1fff)]
            end
        end
        self:wait_one_clock()

        -- when 262, 270, ..., 318
        self:open_pattern(bor(self.io_addr, 8))
        self:wait_one_clock()

        -- when 263, 271, ..., 319
        if self.any_show then
            local buffer_idx = (self.hclk - 263) / 2
            if buffer_idx < self.sp_buffered then
                local pat0 = self.io_pattern
                local pat1 = self.chr_sprite_read(band(self.io_addr, 0x1fff))
                --local pat1 = self.chr_mem[1 + band(self.io_addr, 0x1fff)]
                if pat0 ~= 0 or pat1 ~= 0 then
                    self:load_sprite(pat0, pat1, buffer_idx)
                end
            end
        end
        self:wait_one_clock()
    end
end

function PPU:post_render()
    -- when 681
    self:vblank_0()
    self:wait_zero_clocks()

    -- when 682
    self:vblank_1()
    self:wait_zero_clocks()

    -- when 684
    self:vblank_2()
    self:wait_frame()
end

function PPU:orig_main_loop()
    -- when 685

    -- wait for boot
    self:boot()
    self:wait_frame()
    while true do
        self:pre_render()
        self.on_scanline_0()

        while true do
            self:pre_render_scanline()

            if self.scanline ~= SCANLINE_VBLANK then
                local line
                if self.any_show then
                    line = (self.scanline ~= 0 or not self.odd_frame) and 341 or 340
                else
                    self:update_enabled_flags_edge()
                    line = 341
                end
                self.hclk = 0
                self.vclk = self.vclk + line
                self.hclk_target = self.hclk_target <= line and 0 or self.hclk_target - line
            else
                self.on_scanline_240()
                self.hclk = PPU.HCLOCK_VBLANK_0
                self:wait_zero_clocks()
                break
            end
            self:wait_zero_clocks()

            -- visible scanline (shown)
            self:render_scanline()

            self:post_render_scanline()
        end

        -- post-render scanline (vblank)
        self:post_render()
    end
end

function PPU:main_loop()
    self:pre_render()
    self.on_scanline_0()

    while true do
        self:pre_render_scanline()

        if self.scanline ~= SCANLINE_VBLANK then
            local line
            if self.any_show then
                line = (self.scanline ~= 0 or not self.odd_frame) and 341 or 340
            else
                self:update_enabled_flags_edge()
                line = 341
            end
            self.hclk = 0
            self.vclk = self.vclk + line
            self.hclk_target = self.hclk_target <= line and 0 or self.hclk_target - line
        else
            self.on_scanline_240()
            self.hclk = PPU.HCLOCK_VBLANK_0
            self:wait_zero_clocks()
            break
        end
        self:wait_zero_clocks()

        -- visible scanline (shown)
        self:render_scanline()

        self:post_render_scanline()
    end

    -- post-render scanline (vblank)
    self:post_render()
    return true
end
