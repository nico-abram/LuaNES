local serpent = require("libs/serpent")

local band, bor, bxor, bnot, lshift, rshift = bit.band, bit.bor, bit.bxor, bit.bnot, bit.lshift, bit.rshift
local map, rotatePositiveIdx, nthBitIsSet, nthBitIsSetInt, range, bind =
    UTILS.map,
    UTILS.rotatePositiveIdx,
    UTILS.nthBitIsSet,
    UTILS.nthBitIsSetInt,
    UTILS.range,
    UTILS.bind

ROM = {}
local ROM = ROM
ROM._mt = { __index = ROM }

--[[
    There are different ROM mappers that map the ROM PRG and CHR memory to CP addresses.
    There's a code in the header for each mapper. MAPPER_DB is a mapping of codes into ROM classes.
    The base class uses the most common one (NROM).
]]
ROM.MAPPER_DB = {
    [0x00] = ROM
}

function ROM:initialize(conf, cpu, ppu, basename, bytes, str)
    self.conf = conf or {}
    self.cpu = cpu
    self.ppu =
        ppu or
        {
            set_chr_mem = function()
            end
        }
    self.basename = basename
    local idx = 0
    local prg_count, chr_count, wrk_count
    do
        local header_size = 16
        prg_count, chr_count, wrk_count = self:parse_header({ unpack(bytes, idx + 1, idx + header_size) }, str)
        idx = header_size
    end
    self.prg_banks = {}
    do
        local prg_bank_size = 0x4000
        if #bytes < prg_bank_size * prg_count then
            error "EOF in ROM bank data"
        end
        for i = 1, prg_count do
            self.prg_banks[i] = UTILS.copy(bytes, prg_bank_size, idx)
            idx = idx + prg_bank_size
        end
    end

    self.chr_banks = {}
    do
        local chr_bank_size = 0x2000
        if #bytes < chr_bank_size * chr_count + 0x4000 * prg_count then
            error "EOF in CHR bank data"
        end
        for i = 1, chr_count do
            self.chr_banks[i] = UTILS.copy(bytes, chr_bank_size, idx)
            idx = idx + chr_bank_size
        end
    end
    self.prg_ref = {}
    UTILS.fill(self.prg_ref, 0, 0x10000)
    for i = 0x8000 + 1, 0x4000 + 0x8000 do
        self.prg_ref[i] = self.prg_banks[1][i - 0x8000]
    end
    for i = 0xc000 + 1, 0x4000 + 0xc000 do
        self.prg_ref[i] = self.prg_banks[#(self.prg_banks)][i - 0xc000]
    end
    self.chr_ram = chr_count == 0 -- No CHR bank implies CHR-RAM (writable CHR bank)
    self.chr_ref = self.chr_ram and (UTILS.fill({}, 0, 0x2000)) or UTILS.copy(self.chr_banks[1])

    self.wrk_readable = wrk_count > 0
    self.wrk_writable = false
    self.wrk =
        wrk_count > 0 and
        UTILS.map(
            range(0x6000, 0x7fff),
            function(n)
                return rshift(n, 8)
            end
        ) or
        nil

    self:init()

    self.ppu:nametables(self.mirroring)
    self.ppu:set_chr_mem(self.chr_ref, self.chr_ram)
end

function ROM:init()
end

function ROM:reset()
    self.cpu:add_mappings(range(0x8000, 0xffff), UTILS.tGetter(self.prg_ref), CPU.UNDEFINED)
end

function ROM:peek_6000(addr)
    return self.wrk_readable and self.wrk[addr - 0x6000] or rshift(addr, 8)
end

function ROM:poke_6000(addr, data)
    if self.wrk_writable then
        self.wrk[addr - 0x6000] = data
    end
end

function ROM:vsync()
end

function ROM:load_battery()
    if not self.battery then
        return
    end
    local sav = self.basename .. ".sav"
    local inp = io.open(sav, "rb")
    if not inp then
        return
    end
    self.wrk = serpent.load(inp:read("*all"))
    assert(inp:close())
end

function ROM:save_battery()
    if not self.battery then
        return
    end
    local sav = self.basename .. ".sav"
    print("Saving: " .. sav)
    local out = assert(io.open(sav, "wb"))
    out:write(serpent.dump(self.wrk))
    assert(out:close())
end

function ROM:new(conf, cpu, ppu, basename, bytes, str, custom_metatable)
    local rom = {}
    setmetatable(rom, custom_metatable or ROM._mt)
    rom:initialize(conf, cpu, ppu, basename, bytes, str)
    return rom
end

function ROM.load(conf, cpu, ppu)
    local filename = conf.romfile
    local path, basename, extension = string.match(filename, "(.-)([^\\]-([^\\%.]+))$")

    local inp = assert(io.open(filename, "rb"))
    local str = inp:read("*all")
    assert(inp:close())
    local blob = {}
    for i = 1, str:len() do
        blob[i] = str:byte(i, i)
    end
    local mapper = bor(rshift(blob[7], 4), band(blob[8], 0xf0))

    local klass = ROM.MAPPER_DB[mapper]
    if not klass then
        error(string.format("Unsupported mapper type 0x%02x", mapper))
    end
    return klass:new(conf, cpu, ppu, basename, blob, str)
end

function ROM:parse_header(buf, str)
    if #buf < 16 then
        error "Missing 16-byte header"
    end
    local header = {
        check = str:sub(1, 4),
        trainer = nthBitIsSet(buf[7], 2),
        VS = nthBitIsSet(buf[8], 0),
        PAl = nthBitIsSet(buf[10], 0),
        prg_pages = buf[5],
        chr_pages = buf[6],
        battery = nthBitIsSet(buf[7], 1),
        mapper = bor(rshift(buf[7], 4), band(buf[8], 0xf0)),
        mapping = not nthBitIsSet(buf[7], 0) and "horizontal" or "vertical"
    }
    if header.check ~= "NES\x1a" then
        error "Missing 'NES' constant in header"
    end
    if header.trainer then
        error "trainer not supported"
    end
    if header.VS then
        error "VS cart not supported"
    end
    if header.PAL then
        error "PAL not supported"
    end

    local prg_banks = buf[5] -- program page count
    local chr_banks = buf[6] --vrom page count
    self.mirroring = not nthBitIsSet(buf[7], 0) and "horizontal" or "vertical"
    if nthBitIsSet(buf[7], 3) then
        self.mirroring = "four_screen"
    end
    self.battery = nthBitIsSet(buf[7], 1)
    self.mapper = bor(rshift(buf[7], 4), band(buf[8], 0xf0))
    local ram_banks = math.max(1, buf[9])

    return prg_banks, chr_banks, ram_banks
end

local UxROM = {}
UxROM._mt = { __index = UxROM }
setmetatable(UxROM, { __index = ROM })
function UxROM:new(...)
    local args = { ... }
    table.insert(args, UxROM._mt)
    local rom = ROM:new(unpack(args))
    setmetatable(rom, UxROM._mt)
    return rom
end

function UxROM:reset()
    self.cpu:add_mappings(range(0x8000, 0xffff), UTILS.tGetter(self.prg_ref), bind(self.poke_8000, self))
end

function UxROM:poke_8000(_addr, data)
    local j = band(data, 7)
    for i = 0x8000 + 1, 0x4000 do
        self.prg_ref[i] = self.prg_banks[j][i - 0x8000]
    end
end

ROM.MAPPER_DB[0x02] = UxROM

local CNROM = {}
CNROM._mt = { __index = CNROM }
setmetatable(CNROM, { __index = ROM })
function CNROM:new(...)
    local args = { ... }
    table.insert(args, CNROM._mt)
    local rom = ROM:new(unpack(args))
    setmetatable(rom, CNROM._mt)
    return rom
end

function CNROM:reset()
    self.cpu:add_mappings(
        range(0x8000, 0xffff),
        UTILS.tGetter(self.prg_ref),
        self.chr_ram and bind(self.poke_8000, self) or CPU.UNDEFINED
    )
end

function CNROM:poke_8000(_addr, data)
    local j = band(data, 3)
    self.chr_ref = { unpack(self.chr_banks[j]) }
end

ROM.MAPPER_DB[0x03] = CNROM

local MMC1 = {}
MMC1._mt = { __index = MMC1 }
setmetatable(MMC1, { __index = ROM })
function MMC1:new(...)
    local rom = {}
    setmetatable(rom, MMC1._mt)
    rom:initialize(...)
    return rom
end

local NMT_MODE = { "first", "second", "vertical", "horizontal" }
local PRG_MODE = { "conseq", "conseq", "fix_first", "fix_last" }
local CHR_MODE = { "conseq", "noconseq" }

function MMC1:init()
    self.chr_mode = nil
    self.nmt_mode = nil
    self.prg_mode = nil
    self.prg_bank = 0
    self.chr_bank_0 = 0
    self.chr_bank_1 = 0
end

function MMC1:reset()
    self.shift = 0
    self.shift_count = 0

    --self.chr_banks = self.chr_banks.flatten.each_slice(0x1000).to_a
    local chr = self.chr_banks
    local old = UTILS.copy(self.chr_banks)
    for i = 2, #chr do
        chr[i] = nil
    end
    chr[1] = {}
    local j, k = 1, 1
    for i = 1, #old do
        local bank = old[i]
        for h = 1, #bank do
            local x = bank[h]
            if x then
                chr[j][k] = x
                k = k + 1
                if k == 0x1000 + 1 then
                    j = j + 1
                    k = 1
                    if not chr[j] then
                        chr[j] = {}
                    end
                end
            end
        end
    end

    self.wrk_readable = true
    self.wrk_writable = true
    self.cpu:add_mappings(range(0x6000, 0x7fff), bind(self.peek_6000, self), bind(self.poke_6000, self))
    self.cpu:add_mappings(range(0x8000, 0xffff), UTILS.tGetter(self.prg_ref), bind(self.poke_prg, self))

    self:update_nmt("horizontal")
    self:update_prg("fix_last", 0, 0)
    self:update_chr("conseq", 0, 0)
end

function MMC1:poke_prg(addr, val)
    if nthBitIsSetInt(val, 7) == 1 then
        self.shift = 0
        self.shift_count = 0
        return
    end
    self.shift = bor(self.shift, lshift(nthBitIsSetInt(val, 0), self.shift_count))
    self.shift_count = self.shift_count + 1
    if self.shift_count == 0x05 then
        local x = band(rshift(addr, 13), 0x3)
        if x == 0 then -- control
            local nmt_mode = NMT_MODE[1 + band(self.shift, 3)]
            local prg_mode = PRG_MODE[1 + band(rshift(self.shift, 2), 3)]
            local chr_mode = CHR_MODE[1 + band(rshift(self.shift, 4), 1)]
            self:update_nmt(nmt_mode)
            self:update_prg(prg_mode, self.prg_bank, self.chr_bank_0)
            self:update_chr(chr_mode, self.chr_bank_0, self.chr_bank_1)
        elseif x == 1 then -- change chr_bank_0
            -- update_prg might modify self.chr_bank_0 and prevent updating chr bank,
            -- so keep current value.
            local bak_chr_bank_0 = self.chr_bank_0
            self:update_prg(self.prg_mode, self.prg_bank, self.shift)
            self.chr_bank_0 = bak_chr_bank_0
            self:update_chr(self.chr_mode, self.shift, self.chr_bank_1)
        elseif x == 2 then -- change chr_bank_1
            self:update_chr(self.chr_mode, self.chr_bank_0, self.shift)
        elseif x == 3 then -- change png_bank
            self:update_prg(self.prg_mode, self.shift, self.chr_bank_0)
        end
        self.shift = 0
        self.shift_count = 0
    end
end

function MMC1:update_nmt(nmt_mode)
    if self.nmt_mode == nmt_mode then
        return
    end
    self.nmt_mode = nmt_mode
    self.ppu:nametables(self.nmt_mode)
end

function MMC1:update_prg(prg_mode, prg_bank, chr_bank_0)
    if prg_mode == self.prg_mode and prg_bank == self.prg_bank and chr_bank_0 == self.chr_bank_0 then
        return
    end
    self.prg_mode, self.prg_bank, self.chr_bank_0 = prg_mode, prg_bank, chr_bank_0

    local high_bit = band(chr_bank_0, 0x10, #self.prg_banks - 1)
    local prg_bank_ex = band(bor(band(self.prg_bank, 0x0f), high_bit), #self.prg_banks - 1)
    local lower
    local upper
    if self.prg_mode == "conseq" then
        lower = band(prg_bank_ex, bnot(1))
        upper = lower + 1
    elseif self.prg_mode == "fix_first" then
        lower = 0
        upper = prg_bank_ex
    elseif self.prg_mode == "fix_last" then
        lower = prg_bank_ex
        upper = bor(band(#self.prg_banks - 1, 0x0f), high_bit)
    end
    for i = 1, 0x4000 + 1 do
        self.prg_ref[i + 0x8000] = self.prg_banks[1 + lower][i]
    end
    for i = 1, 0x4000 + 1 do
        self.prg_ref[i + 0xc000] = self.prg_banks[1 + upper][i]
    end
end

function MMC1:update_chr(chr_mode, chr_bank_0, chr_bank_1)
    if chr_mode == self.chr_mode and chr_bank_0 == self.chr_bank_0 and chr_bank_1 == self.chr_bank_1 then
        return
    end
    self.chr_mode, self.chr_bank_0, self.chr_bank_1 = chr_mode, chr_bank_0, chr_bank_1
    if self.chr_ram then
        return
    end
    local lower
    local upper
    self.ppu:update(0)
    if self.chr_mode == "conseq" then
        lower = band(self.chr_bank_0, 0x1e) + 1
        upper = lower + 1
    else
        lower = self.chr_bank_0 + 1
        upper = self.chr_bank_1 + 1
    end
    for i = 1, 0x1000 do
        self.chr_ref[i] = self.chr_banks[lower][i]
    end
    for i = 1, 0x1000 do
        self.chr_ref[i + 0x1000] = self.chr_banks[upper][i]
    end
end

ROM.MAPPER_DB[0x01] = MMC1

local MMC3 = {}
MMC3._mt = { __index = MMC3 }
setmetatable(MMC3, { __index = ROM })
function MMC3:new(...)
    local args = { ... }
    table.insert(args, MMC3._mt)
    local rom = ROM:new(unpack(args))
    setmetatable(rom, MMC3._mt)
    return rom
end

function MMC3:init(rev) -- rev = :A or :B or :C
    rev = rev or "B"
    self.persistant = rev ~= "A"

    --self.prg_banks = self.prg_banks.flatten.each_slice(0x2000).to_a
    local new_banks = { {} }
    local new_bank = new_banks[1]
    local new_bank_i = 1
    local new_bank_j = 1
    for i = 1, #(self.prg_banks) do
        local old_bank = self.prg_banks[i]
        for j = 1, #old_bank do
            if new_bank_j == 0x2001 then
                new_bank_j = 1
                new_bank_i = new_bank_i + 1
                new_bank = {}
                new_banks[new_bank_i] = new_bank
            end
            new_bank[new_bank_j] = old_bank[j]
            new_bank_j = new_bank_j + 1
        end
    end
    self.prg_banks = new_banks
    self.prg_bank_swap = false

    --self.chr_banks = self.chr_banks.flatten.each_slice(0x0400).to_a
    local new_banks = { {} }
    local new_bank = new_banks[1]
    local new_bank_i = 1
    local new_bank_j = 1
    for i = 1, #(self.chr_banks) do
        local old_bank = self.chr_banks[i]
        for j = 1, #old_bank do
            if new_bank_j == 0x0401 then
                new_bank_j = 1
                new_bank_i = new_bank_i + 1
                new_bank = {}
                new_banks[new_bank_i] = new_bank
            end
            new_bank[new_bank_j] = old_bank[j]
            new_bank_j = new_bank_j + 1
        end
    end
    self.chr_banks = new_banks
    self.chr_bank_mapping = UTILS.fill({}, nil, 8)
    self.chr_bank_swap = false
end

function MMC3:reset()
    self.wrk_readable = true
    self.wrk_writable = false

    local poke_a000 = self.mirroring ~= "FourScreen" and bind(self.poke_a000, self) or CPU.UNDEFINED
    self.cpu:add_mappings(range(0x6000, 0x7fff), bind(self.peek_6000, self), bind(self.poke_6000, self))
    local g = UTILS.tGetter(self.prg_ref)
    self.cpu:add_mappings(range(0x8000, 0x9fff, 2), g, bind(self.poke_8000, self))
    self.cpu:add_mappings(range(0x8001, 0x9fff, 2), g, bind(self.poke_8001, self))
    self.cpu:add_mappings(range(0xa000, 0xbfff, 2), g, poke_a000)
    self.cpu:add_mappings(range(0xa001, 0xbfff, 2), g, bind(self.poke_a001, self))
    self.cpu:add_mappings(range(0xc000, 0xdfff, 2), g, bind(self.poke_c000, self))
    self.cpu:add_mappings(range(0xc001, 0xdfff, 2), g, bind(self.poke_c001, self))
    self.cpu:add_mappings(range(0xe000, 0xffff, 2), g, bind(self.poke_e000, self))
    self.cpu:add_mappings(range(0xe001, 0xffff, 2), g, bind(self.poke_e001, self))

    self:update_prg(0x8000, 0)
    self:update_prg(0xa000, 1)
    self:update_prg(0xc000, -2)
    self:update_prg(0xe000, -1)
    for i = 0, 7 do
        self:update_chr(i * 0x400, i)
    end

    self.clock = 0
    if PPU then
        self.hold = PPU.RP2C02_CC * 16
    end
    self.ppu:monitor_a12_rising_edge(self)
    self.cpu.ppu_sync = true

    self.count = 0
    self.latch = 0
    self.reload = false
    self.enabled = false
end

-- prg_bank_swap = F T
-- 0x8000..0x9fff: 0 2
-- 0xa000..0xbfff: 1 1
-- 0xc000..0xdfff: 2 0
-- 0xe000..0xffff: 3 3
function MMC3:update_prg(addr, bank)
    bank = bank % (#self.prg_banks)
    if self.prg_bank_swap and band(addr[13], 0x2000) == 0 then
        addr = bxor(addr, 0x4000)
    end
    --self.prg_ref[1 + bank] = self.prg_banks[1 + bank]
    for i = addr + 1, addr + 0x2000 do
        self.prg_ref[i] = self.prg_banks[1 + bank][i - addr]
    end
end

function MMC3:update_chr(addr, bank)
    if self.chr_ram then
        return
    end
    local idx = addr / 0x400
    bank = bank % (#self.chr_banks)
    if self.chr_bank_mapping[idx + 1] == bank then
        return
    end
    if self.chr_bank_swap then
        addr = bxor(addr, 0x1000)
    end
    self.ppu:update(0)
    for i = 1, 0x400 do
        self.chr_ref[i + addr] = self.chr_banks[bank + 1][i]
    end
    self.chr_bank_mapping[idx] = bank
end

function MMC3:poke_8000(_addr, data)
    self.reg_select = band(data, 7)
    local prg_bank_swap = band(data, 0x40) == 0x40
    local chr_bank_swap = band(data, 0x80) == 0x80

    if prg_bank_swap ~= self.prg_bank_swap then
        self.prg_bank_swap = prg_bank_swap
        UTILS.swapRanges(self.prg_ref, 0x8000, 0xc000)
    end

    if chr_bank_swap ~= self.chr_bank_swap then
        self.chr_bank_swap = chr_bank_swap
        if not self.chr_ram then
            self.ppu:update(0)
            self.chr_ref = UTILS.rotate(self.chr_ref, 0x1000)
            self.chr_bank_mapping = UTILS.rotate(self.chr_bank_mapping, 4)
        end
    end
end

function MMC3:poke_8001(_addr, data)
    if self.reg_select < 6 then
        if self.reg_select < 2 then
            self:update_chr(self.reg_select * 0x0800, band(data, 0xfe))
            self:update_chr(self.reg_select * 0x0800 + 0x0400, bor(data, 0x01))
        else
            self:update_chr((self.reg_select - 2) * 0x0400 + 0x1000, data)
        end
    else
        self:update_prg((self.reg_select - 6) * 0x2000 + 0x8000, band(data, 0x3f))
    end
end

function MMC3:poke_a000(_addr, data)
    self.ppu:nametables(band(data, 0x1) == 0x1 and "horizontal" or "vertical")
end

function MMC3:poke_a001(_addr, data)
    self.wrk_readable = band(data, 0x80) == 0x80
    self.wrk_writable = band(data, 0x40) == 0x0 and self.wrk_readable
end

function MMC3:poke_c000(_addr, data)
    self.ppu:update(0)
    self.latch = data
end

function MMC3:poke_c001(_addr, _data)
    self.ppu:update(0)
    self.reload = true
end

function MMC3:poke_e000(_addr, _data)
    self.ppu:update(0)
    self.enabled = false
    self.cpu:clear_irq(CPU.IRQ_EXT)
end

function MMC3:poke_e001(_addr, _data)
    self.ppu:update(0)
    self.enabled = true
end

function MMC3:vsync()
    self.clock = self.clock > self.cpu:next_frame_clock() and self.clock - self.cpu:next_frame_clock() or 0
end

function MMC3:a12_signaled(cycle)
    local clk = self.clock
    self.clock = cycle + self.hold
    if cycle < clk then
        return
    end
    local flag = self.persistant or self.count > 0
    if self.reload then
        self.reload = false
        self.count = self.latch
    elseif self.count == 0 then
        self.count = self.latch
    else
        self.count = self.count - 1
    end
    if flag and self.count == 0 and self.enabled then
        self.cpu:do_irq(CPU.IRQ_EXT, cycle)
    end
end

ROM.MAPPER_DB[0x04] = MMC3

local MMC5 = {}
MMC5._mt = { __index = MMC5 }
setmetatable(MMC5, { __index = ROM })
function MMC5:new(...)
    local args = { ... }
    table.insert(args, MMC5._mt)
    local rom = ROM:new(unpack(args))
    setmetatable(rom, MMC5._mt)
    return rom
end

function MMC5:init()
    self.chr_mem = {}                                                                                     -- ??K CHR ROM
    --self.prg_ram = UTILS.fill({}, 0x00, 0x10000)
    self.prg_ram = UTILS.map(UTILS.fill({}, 0x00, 8), function() return UTILS.fill({}, 0x00, 0x2000) end) -- 64K PRG RAM

    -- Reorganize 8k chr ROM banks into 1kb banks
    do
        local initial_chr_banks = self.chr_banks
        self.chr_1k_banks = {}
        local chr_rom_bank_idx = 0
        local chr_rom_idx = (1024) + 1
        for i = 1, #initial_chr_banks do
            local old_chr_bank = initial_chr_banks[i]
            for j = 1, #old_chr_bank do
                if chr_rom_idx == ((1024) + 1) then
                    chr_rom_idx = 1
                    chr_rom_bank_idx = chr_rom_bank_idx + 1
                    self.chr_1k_banks[chr_rom_bank_idx] = {}
                end
                self.chr_1k_banks[chr_rom_bank_idx][chr_rom_idx - 1] = old_chr_bank[j]
                chr_rom_idx                                          = chr_rom_idx + 1
            end
        end
    end
    -- Reorganize 1-indexed 16k prg ROM banks into 0-indexed 8kb banks
    do
        local initial_16k_prg_rom_banks = self.prg_banks
        self.prg_banks = {}
        local prg_rom_bank_idx = 0
        local prg_rom_idx = (8 * 1024) + 1
        for i = 1, #initial_16k_prg_rom_banks do
            local old_prg_bank = initial_16k_prg_rom_banks[i]
            for j = 1, #old_prg_bank do
                if prg_rom_idx == ((8 * 1024) + 1) then
                    prg_rom_idx = 1
                    prg_rom_bank_idx = prg_rom_bank_idx + 1
                    self.prg_banks[prg_rom_bank_idx] = {}
                end
                self.prg_banks[prg_rom_bank_idx][prg_rom_idx - 1] = old_prg_bank[j]
                prg_rom_idx                                       = prg_rom_idx + 1
            end
        end
    end

    self.chr_mode                   = 0x00
    self.chr_sprite_page_selectors  = { 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F } -- chr page "A"
    self.chr_bg_page_selectors      = { 0x7F, 0x7F, 0x7F, 0x7F }                         -- chr page "B"
    self.chr_selector_high_bits     = 0x00                                               -- top 2 bits of the 10 bit selector

    -- "The Koei games never write to this register, apparently relying on the MMC5 defaulting to mode 3 at power on. "
    -- -nesdev wiki
    self.prg_mode                   = 0x03
    self.prg_selectors              = { 0x00, 0x00, 0x00, 0x00, #(self.prg_banks) - 1 }
    local last_prg_bank             = self.prg_banks[#(self.prg_banks)]
    self.prg_ref_8k_banks           = { 0x00, last_prg_bank, last_prg_bank, last_prg_bank, last_prg_bank }

    self.ram_protect1               = 0x00
    self.ram_protect2               = 0x00

    self.internal_ram_extended_mode = 0x00 -- starting value??

    self.ppu_nametable_mappings_reg = 0x00
    self.ppu_nametable_mappings     = { 0x00, 0x00, 0x00, 0x00 } -- starting value??
    self.nametable_fill_mode_tile   = 0x00
    self.nametable_fill_mode_color  = 0x00

    self.multiplicand               = 0xFF
    self.multiplier                 = 0xFF

    self.scanline_pending_bit       = 0x00
    self.in_frame_bit               = 0x00
    self.irq_enable                 = false
    self.scanline_counter           = 0x00

    local b                         = self.chr_1k_banks[1]
    self.chr_ref_sp_1k_chr_banks    = { b, b, b, b, b, b, b, b }
    self.chr_ref_bg_1k_chr_banks    = { b, b, b, b }
    self.chr_ref_last_1k_chr_banks  = self.chr_ref_sp_1k_chr_banks

    self.chr_ref                    = setmetatable({},
        { __index = bind(self.chr_read, self), __newindex = bind(self.chr_write, self) })
end

function MMC5:get_prg_8k_bank(selector)
    local is_ram = band(selector, 0x80) == 0
    local ram_mask = band(selector, 0x07)
    local rom_mask = band(selector, 0x7F)
    return is_ram and self.prg_ram[(ram_mask % #(self.prg_ram)) + 1] or
        self.prg_banks[(rom_mask % #(self.prg_banks)) + 1]
end

function MMC5:update_prg()
    self.prg_ref_8k_banks[1] = self:get_prg_8k_bank(band(self.prg_selectors[1], 0x7))

    local mode = band(self.prg_mode, 0x3)
    if mode == 0x00 then
        local masked = bor(band(self.prg_selectors[5], 0x7C), 0x80)
        self.prg_ref_8k_banks[2] = self:get_prg_8k_bank(masked)
        self.prg_ref_8k_banks[3] = self:get_prg_8k_bank(masked + 1)
        self.prg_ref_8k_banks[4] = self:get_prg_8k_bank(masked + 2)
        self.prg_ref_8k_banks[5] = self:get_prg_8k_bank(masked + 3)
    elseif mode == 0x01 then
        local masked = band(self.prg_selectors[3], 0xFE)
        self.prg_ref_8k_banks[2] = self:get_prg_8k_bank(masked)
        self.prg_ref_8k_banks[3] = self:get_prg_8k_bank(masked + 1)
        local masked2 = bor(band(self.prg_selectors[5], 0x7E), 0x80)
        self.prg_ref_8k_banks[4] = self:get_prg_8k_bank(masked2)
        self.prg_ref_8k_banks[5] = self:get_prg_8k_bank(masked2 + 1)
    elseif mode == 0x02 then
        local masked = band(self.prg_selectors[3], 0xFE)
        self.prg_ref_8k_banks[2] = self:get_prg_8k_bank(masked)
        self.prg_ref_8k_banks[3] = self:get_prg_8k_bank(masked + 1)
        self.prg_ref_8k_banks[4] = self:get_prg_8k_bank(self.prg_selectors[4])
        self.prg_ref_8k_banks[5] = self:get_prg_8k_bank(bor(self.prg_selectors[5], 0x80))
    elseif mode == 0x03 then
        self.prg_ref_8k_banks[2] = self:get_prg_8k_bank(self.prg_selectors[2])
        self.prg_ref_8k_banks[3] = self:get_prg_8k_bank(self.prg_selectors[3])
        self.prg_ref_8k_banks[4] = self:get_prg_8k_bank(self.prg_selectors[4])
        self.prg_ref_8k_banks[5] = self:get_prg_8k_bank(bor(self.prg_selectors[5], 0x80))
    else
        assert(false)
    end
end

function MMC5:add_register(addr, field, on_set)
    local setter = on_set and function(_i, v)
        self[field] = v
        on_set(v)
    end or function(_i, v) self[field] = v end
    self.cpu:add_mappings(addr, function() return self[field] end, setter)
end

function MMC5:reset()
    self:add_register(0x5101, 'chr_mode', bind(self.update_chr_1k_banks, self))

    do
        local chr_sp_pg_idxs = self.chr_sprite_page_selectors
        local chr_bg_pg_idxs = self.chr_bg_page_selectors
        local chr_sp_ref = self.chr_ref_sp_1k_chr_banks
        local chr_bg_ref = self.chr_ref_bg_1k_chr_banks

        self.cpu:add_mappings(range(0x5120, 0x5127), function(i)
                return chr_sp_pg_idxs[i - 0x511F]
            end,
            function(i, v)
                self.chr_ref_last_1k_chr_banks = chr_sp_ref
                chr_sp_pg_idxs[i - 0x511F] = v + lshift(band(self.chr_selector_high_bits, 0x3), 8)
                self:update_chr_1k_banks()
            end)
        self.cpu:add_mappings(range(0x5128, 0x512B), function(i, v)
                return chr_bg_pg_idxs[i - 0x5127]
            end,
            function(i, v)
                self.chr_ref_last_1k_chr_banks = chr_bg_ref
                chr_bg_pg_idxs[i - 0x5127] = v + lshift(band(self.chr_selector_high_bits, 0x3), 8)
                self:update_chr_1k_banks()
            end)
    end
    self:add_register(0x5130, 'chr_selector_high_bits')

    self:add_register(0x5100, 'prg_mode', bind(self.update_prg, self))
    do
        local prg_selectors = self.prg_selectors
        self.cpu:add_mappings(range(0x5113, 0x5117), function(i)
                return prg_selectors[i - 0x5112]
            end,
            function(i, v)
                prg_selectors[i - 0x5112] = v
                self:update_prg()
            end)
    end
    local map_prg_bank = function(bank_num, offset)
        self.cpu:add_mappings(range(offset, offset + 0x1FFF), function(i)
                return self.prg_ref_8k_banks[bank_num][band(i, 0x1FFF)]
            end,
            function(i, v)
                self.prg_ref_8k_banks[bank_num][band(i, 0x1FFF)] = v
            end)
    end
    map_prg_bank(1, 0x6000)
    map_prg_bank(2, 0x8000)
    map_prg_bank(3, 0xA000)
    map_prg_bank(4, 0xC000)
    map_prg_bank(5, 0xE000)

    self:add_register(0x5102, 'ram_protect1')
    self:add_register(0x5103, 'ram_protect2')

    do
        local ppu_nt = self.ppu_nametable_mappings
        self:add_register(0x5105, 'ppu_nametable_mappings_reg', function(reg)
            self.ppu_nametable_mappings[1] = band(reg, 0x3)
            self.ppu_nametable_mappings[2] = band(rshift(reg, 2), 0x3)
            self.ppu_nametable_mappings[3] = band(rshift(reg, 4), 0x3)
            self.ppu_nametable_mappings[4] = band(rshift(reg, 6), 0x3)
            -- TODO: Unimplemented nametable mappings 2 and 3 (Extended RAM and fill-mode)
            --assert(self.ppu_nametable_mappings[1] <= 0x1)
            --assert(self.ppu_nametable_mappings[2] <= 0x1)
            --assert(self.ppu_nametable_mappings[3] <= 0x1)
            --assert(self.ppu_nametable_mappings[4] <= 0x1)
            -- HACK: ignore NT 3 and 4
            self.ppu_nametable_mappings[1] = band(reg, 0x1)
            self.ppu_nametable_mappings[2] = band(rshift(reg, 2), 0x1)
            self.ppu_nametable_mappings[3] = band(rshift(reg, 4), 0x1)
            self.ppu_nametable_mappings[4] = band(rshift(reg, 6), 0x1)
            self.ppu:nametables(self.ppu_nametable_mappings)
        end)
    end

    self:add_register(0x5106, 'nametable_fill_mode_tile')
    self:add_register(0x5107, 'nametable_fill_mode_color')

    self.cpu:add_mappings(0x5205, function(i, v)
            return band(self.multiplicand * self.multiplier, 0xFF)
        end,
        function(i, v)
            self.multiplicand = v
        end)
    self.cpu:add_mappings(0x5206, function(i, v)
            return band(rshift(self.multiplicand * self.multiplier, 8), 0xFF)
        end,
        function(i, v)
            self.multiplier = v
        end)

    self.cpu.ppu_sync = true
    local scanline_counter_callback = bind(self.scanline_counter_callback, self)
    self:add_register(0x5203, 'scanline_counter',
        function(v)
            self.ppu:scanline_counter_listener(v, scanline_counter_callback)
        end)
    -- TODO: Better In Frame bit
    self.cpu:add_mappings(0x5204, function(i, v)
            local ret = bor(self.in_frame_bit, self.scanline_pending_bit)
            self.scanline_pending_bit = 0x00
            self.cpu:clear_irq(CPU.IRQ_EXT)
            return ret
        end,
        function(i, v)
            self.irq_enable = band(v, 0x80) ~= 0x00
        end)

    -- TODO: ExAttribute mode
    self.ppu.chr_peek = function(idx)
        local addr_1kpage_idx = rshift(idx, 10) -- / 0x400
        return self.chr_ref_last_1k_chr_banks[addr_1kpage_idx + 1][band(idx, 0x3FF)]
    end
    self.ppu.chr_poke = function(idx, val)
        local addr_1kpage_idx = rshift(idx, 10) -- / 0x400
        self.chr_ref_last_1k_chr_banks[addr_1kpage_idx + 1][band(idx, 0x3FF)] = val
    end
    self.ppu.chr_bg_read = function(idx)
        local addr_1kpage_idx = band(rshift(idx, 10), 0x3) -- / 0x400
        return self.chr_ref_bg_1k_chr_banks[addr_1kpage_idx + 1][band(idx, 0x3FF)]
    end
    self.ppu.chr_sprite_read = function(idx)
        local addr_1kpage_idx = rshift(idx, 10) -- / 0x400
        return self.chr_ref_sp_1k_chr_banks[addr_1kpage_idx + 1][band(idx, 0x3FF)]
    end
    self.ppu.on_scanline_0 = function()
        self.in_frame_bit = 0x00
    end
    self.ppu.on_scanline_240 = function()
        self.in_frame_bit = 0x40
    end
end

function MMC5:update_chr_1k_banks()
    local sp_selectrs = self.chr_sprite_page_selectors
    local bg_selectrs = self.chr_bg_page_selectors
    local bg_ref = self.chr_ref_bg_1k_chr_banks
    local sp_ref = self.chr_ref_sp_1k_chr_banks
    local n_chr_banks = #(self.chr_1k_banks)

    if self.chr_mode == 0x03 then
        sp_ref[1] = self.chr_1k_banks[(sp_selectrs[1] % n_chr_banks) + 1]
        sp_ref[2] = self.chr_1k_banks[(sp_selectrs[2] % n_chr_banks) + 1]
        sp_ref[3] = self.chr_1k_banks[(sp_selectrs[3] % n_chr_banks) + 1]
        sp_ref[4] = self.chr_1k_banks[(sp_selectrs[4] % n_chr_banks) + 1]
        sp_ref[5] = self.chr_1k_banks[(sp_selectrs[5] % n_chr_banks) + 1]
        sp_ref[6] = self.chr_1k_banks[(sp_selectrs[6] % n_chr_banks) + 1]
        sp_ref[7] = self.chr_1k_banks[(sp_selectrs[7] % n_chr_banks) + 1]
        sp_ref[8] = self.chr_1k_banks[(sp_selectrs[8] % n_chr_banks) + 1]

        bg_ref[1] = self.chr_1k_banks[(bg_selectrs[1] % n_chr_banks) + 1]
        bg_ref[2] = self.chr_1k_banks[(bg_selectrs[2] % n_chr_banks) + 1]
        bg_ref[3] = self.chr_1k_banks[(bg_selectrs[3] % n_chr_banks) + 1]
        bg_ref[4] = self.chr_1k_banks[(bg_selectrs[4] % n_chr_banks) + 1]
    elseif self.chr_mode == 0x02 then
        local bank_idx_sp = bg_selectrs[2] * 2
        sp_ref[1] = self.chr_1k_banks[((bank_idx_sp) % n_chr_banks) + 1]
        sp_ref[2] = self.chr_1k_banks[((bank_idx_sp + 1) % n_chr_banks) + 1]
        local bank_idx_sp2 = bg_selectrs[4] * 2
        sp_ref[3] = self.chr_1k_banks[((bank_idx_sp2) % n_chr_banks) + 1]
        sp_ref[4] = self.chr_1k_banks[((bank_idx_sp2 + 1) % n_chr_banks) + 1]
        local bank_idx_sp3 = bg_selectrs[6] * 2
        sp_ref[5] = self.chr_1k_banks[((bank_idx_sp3) % n_chr_banks) + 1]
        sp_ref[6] = self.chr_1k_banks[((bank_idx_sp3 + 1) % n_chr_banks) + 1]
        local bank_idx_sp4 = bg_selectrs[8] * 2
        sp_ref[7] = self.chr_1k_banks[((bank_idx_sp4) % n_chr_banks) + 1]
        sp_ref[8] = self.chr_1k_banks[((bank_idx_sp4 + 1) % n_chr_banks) + 1]

        local bank_idx_sp = bg_selectrs[2] * 2
        bg_ref[1] = self.chr_1k_banks[((bank_idx_sp) % n_chr_banks) + 1]
        bg_ref[2] = self.chr_1k_banks[((bank_idx_sp + 1) % n_chr_banks) + 1]
        local bank_idx_sp2 = bg_selectrs[4] * 2
        bg_ref[3] = self.chr_1k_banks[((bank_idx_sp2) % n_chr_banks) + 1]
        bg_ref[4] = self.chr_1k_banks[((bank_idx_sp2 + 1) % n_chr_banks) + 1]
    elseif self.chr_mode == 0x01 then
        local bank_idx = sp_selectrs[4] * 4
        sp_ref[1] = self.chr_1k_banks[((bank_idx) % n_chr_banks) + 1]
        sp_ref[2] = self.chr_1k_banks[((bank_idx + 1) % n_chr_banks) + 1]
        sp_ref[3] = self.chr_1k_banks[((bank_idx + 2) % n_chr_banks) + 1]
        sp_ref[4] = self.chr_1k_banks[((bank_idx + 3) % n_chr_banks) + 1]
        local bank_idx2 = sp_selectrs[8] * 4
        sp_ref[5] = self.chr_1k_banks[((bank_idx2) % n_chr_banks) + 1]
        sp_ref[6] = self.chr_1k_banks[((bank_idx2 + 1) % n_chr_banks) + 1]
        sp_ref[7] = self.chr_1k_banks[((bank_idx2 + 2) % n_chr_banks) + 1]
        sp_ref[8] = self.chr_1k_banks[((bank_idx2 + 3) % n_chr_banks) + 1]

        local bank_idx_sp = bg_selectrs[4] * 4
        bg_ref[1] = self.chr_1k_banks[((bank_idx_sp) % n_chr_banks) + 1]
        bg_ref[2] = self.chr_1k_banks[((bank_idx_sp + 1) % n_chr_banks) + 1]
        bg_ref[3] = self.chr_1k_banks[((bank_idx_sp + 2) % n_chr_banks) + 1]
        bg_ref[4] = self.chr_1k_banks[((bank_idx_sp + 3) % n_chr_banks) + 1]
    elseif self.chr_mode == 0x00 then
        local bank_idx = sp_selectrs[8] * 8
        sp_ref[1] = self.chr_1k_banks[((bank_idx) % n_chr_banks) + 1]
        sp_ref[2] = self.chr_1k_banks[((bank_idx + 1) % n_chr_banks) + 1]
        sp_ref[3] = self.chr_1k_banks[((bank_idx + 2) % n_chr_banks) + 1]
        sp_ref[4] = self.chr_1k_banks[((bank_idx + 3) % n_chr_banks) + 1]
        sp_ref[5] = self.chr_1k_banks[((bank_idx + 4) % n_chr_banks) + 1]
        sp_ref[6] = self.chr_1k_banks[((bank_idx + 5) % n_chr_banks) + 1]
        sp_ref[7] = self.chr_1k_banks[((bank_idx + 6) % n_chr_banks) + 1]
        sp_ref[8] = self.chr_1k_banks[((bank_idx + 7) % n_chr_banks) + 1]

        local bank_idx_sp = bg_selectrs[4] * 8
        bg_ref[1] = self.chr_1k_banks[((bank_idx_sp) % n_chr_banks) + 1]
        bg_ref[2] = self.chr_1k_banks[((bank_idx_sp + 1) % n_chr_banks) + 1]
        bg_ref[3] = self.chr_1k_banks[((bank_idx_sp + 2) % n_chr_banks) + 1]
        bg_ref[4] = self.chr_1k_banks[((bank_idx_sp + 3) % n_chr_banks) + 1]
    end
end

function MMC5:scanline_counter_callback()
    if self.irq_enable then
        -- TODO: Clock this better?
        local clk = self.cpu:current_clock();
        (self.cpu):do_irq(CPU.IRQ_EXT, clk)
    end
end

ROM.MAPPER_DB[0x05] = MMC5

local AxROM = {}
AxROM._mt = { __index = AxROM }
setmetatable(AxROM, { __index = ROM })
function AxROM:new(...)
    local args = { ... }
    table.insert(args, AxROM._mt)
    local rom = ROM:new(unpack(args))
    setmetatable(rom, AxROM._mt)
    return rom
end

function AxROM:reset()
    self.prg_banks = UTILS.flattenSplit(self.prg_banks, 1024 * 32)
    self.current_prg_bank = self.prg_banks[1]
    local write_prg = function(_i, v)
        self.prg_bank_idx = band(v, 0x7) % #(self.prg_banks)
        self.current_prg_bank = self.prg_banks[self.prg_bank_idx + 1]
        self.ppu:nametables(band(v, 0x10) == 0 and "first" or "second")
    end
    local read_prg = function(i)
        return self.current_prg_bank[i - 0x8000 + 1]
    end

    self.cpu:add_mappings(range(0x8000, 0xffff), read_prg, write_prg)
end

ROM.MAPPER_DB[0x07] = AxROM

local ColorDreamsROM = {}
ColorDreamsROM._mt = { __index = ColorDreamsROM }
setmetatable(ColorDreamsROM, { __index = ROM })
function ColorDreamsROM:new(...)
    local args = { ... }
    table.insert(args, ColorDreamsROM._mt)
    local rom = ROM:new(unpack(args))
    setmetatable(rom, ColorDreamsROM._mt)
    return rom
end

function ColorDreamsROM:reset()
    self.chr_banks           = UTILS.flattenSplit(self.chr_banks, 1024 * 8)
    self.prg_banks           = UTILS.flattenSplit(self.prg_banks, 1024 * 32)

    self.current_prg_bank    = self.prg_banks[1]
    self.current_chr_bank    = self.chr_banks[1]
    local write_prg          = function(_i, v)
        self.prg_bank_idx = band(v, 0x3) % #(self.prg_banks)
        self.chr_bank_idx = band(rshift(v, 4), 0xF) % #(self.chr_banks)
        self.current_prg_bank = self.prg_banks[self.prg_bank_idx + 1]
        self.current_chr_bank = self.chr_banks[self.chr_bank_idx + 1]
    end
    local read_prg           = function(i)
        return self.current_prg_bank[i - 0x8000 + 1]
    end
    self.ppu.chr_peek        = function(i)
        return self.current_chr_bank[i + 1]
    end
    self.ppu.chr_poke        = function(_i, _v)
    end
    self.ppu.chr_bg_read     = self.ppu.chr_peek
    self.ppu.chr_sprite_read = self.ppu.chr_peek

    self.cpu:add_mappings(range(0x8000, 0xffff), read_prg, write_prg)
end

ROM.MAPPER_DB[0x0B] = ColorDreamsROM

local GxROM = {}
GxROM._mt = { __index = GxROM }
setmetatable(GxROM, { __index = ROM })
function GxROM:new(...)
    local args = { ... }
    table.insert(args, GxROM._mt)
    local rom = ROM:new(unpack(args))
    setmetatable(rom, GxROM._mt)
    return rom
end

function GxROM:reset()
    self.prg_banks           = UTILS.flattenSplit(self.prg_banks, 1024 * 32)

    self.current_prg_bank    = self.prg_banks[1]
    self.current_chr_bank    = self.chr_banks[1]
    local write_prg          = function(_i, v)
        self.chr_bank_idx = band(v, 0x3) % #(self.chr_banks)
        self.prg_bank_idx = band(rshift(v, 4), 0x3) % #(self.prg_banks)
        self.current_prg_bank = self.prg_banks[self.prg_bank_idx + 1]
        self.current_chr_bank = self.chr_banks[self.chr_bank_idx + 1]
    end
    local read_prg           = function(i)
        return self.current_prg_bank[i - 0x8000 + 1]
    end
    self.ppu.chr_peek        = function(i)
        return self.current_chr_bank[i + 1]
    end
    self.ppu.chr_poke        = function(i, v)
        --self.current_chr_bank[i + 1] = v
    end
    self.ppu.chr_bg_read     = self.ppu.chr_peek
    self.ppu.chr_sprite_read = self.ppu.chr_peek

    self.cpu:add_mappings(range(0x8000, 0xffff), read_prg, write_prg)
end

ROM.MAPPER_DB[0x42] = GxROM

local MMC2 = {}
MMC2._mt = { __index = MMC2 }
setmetatable(MMC2, { __index = ROM })
function MMC2:new(...)
    local args = { ... }
    table.insert(args, MMC2._mt)
    local rom = ROM:new(unpack(args))
    setmetatable(rom, MMC2._mt)
    return rom
end

function MMC2:reset()
    self.wrk     = nil
    self.prg_ram = UTILS.fill({}, 0x00, 1024 * 8) -- 8K PRG RAM
    self.cpu:add_mappings(range(0x6000, 0x7fff), UTILS.tGetter(self.prg_ram, 1 - 0x6000),
        UTILS.tSetter(self.prg_ram, 1 - 0x6000))

    self.chr_banks            = UTILS.flattenSplit(self.chr_banks, 1024 * 4)
    self.prg_banks            = UTILS.flattenSplit(self.prg_banks, 1024 * 8)

    self.chr_bank_idxs        = { { 1, 1 }, { 1, 1 } } -- idxs into chr_banks
    self.current_chr_banks    = { 1, 1 }               -- idxs into chr_bank_idxs[1] and [2]
    self.ppu.chr_peek         = function(addr)
        local masked = band(addr, 0xFFF)               -- addr within bank
        local page = band(rshift(addr, 12), 0x1) + 1   -- address page 1 or 2

        local latch = self.current_chr_banks[page]
        local bank_idx = self.chr_bank_idxs[page][latch]
        local bank = self.chr_banks[bank_idx]

        if addr == 0x0FD8 then
            self.current_chr_banks[1] = 1
        elseif addr == 0x0FE8 then
            self.current_chr_banks[1] = 2
        elseif band(addr, 0x1FF8) == 0x1FD8 then
            self.current_chr_banks[2] = 1
        elseif band(addr, 0x1FF8) == 0x1FE8 then
            self.current_chr_banks[2] = 2
        end

        return bank[masked + 1]
    end
    self.ppu.chr_poke         = function(i, v)
    end
    self.ppu.chr_bg_read      = self.ppu.chr_peek
    self.ppu.chr_sprite_read  = self.ppu.chr_peek
    local select_chr          = function(reg_num)
        local idxs_table = reg_num <= 2 and self.chr_bank_idxs[1] or self.chr_bank_idxs[2]
        local idx_in_idxs_table = reg_num > 2 and reg_num - 2 or reg_num
        return function(_i, v)
            idxs_table[idx_in_idxs_table] = band(v, 0x1F) + 1
        end
    end

    local read_switchable_prg = function(i)
        return self.current_prg_bank[i - 0x8000 + 1]
    end
    local read_fixed_prg      = function(i)
        local addr = i - 0xA000
        local bank = rshift(addr, 13)
        bank       = #(self.prg_banks) - 2 + bank
        addr       = band(addr, 0x1FFF)
        return self.prg_banks[bank][addr + 1]
    end
    local write_prg           = function(_i, v)
        self.prg_bank_idx = band(v, 0xF) % #(self.prg_banks)
        self.current_prg_bank = self.prg_banks[self.prg_bank_idx + 1]
    end
    local mirroring           = function(_i, v)
        self.ppu:nametables(band(v, 0x01) == 0x01 and "horizontal" or "vertical")
    end
    self.cpu:add_mappings(range(0x8000, 0x9FFF), read_switchable_prg, CPU.UNDEFINED)
    self.cpu:add_mappings(range(0xA000, 0xAFFF), read_fixed_prg, write_prg)
    self.cpu:add_mappings(range(0xB000, 0xBFFF), read_fixed_prg, select_chr(1))
    self.cpu:add_mappings(range(0xC000, 0xCFFF), read_fixed_prg, select_chr(2))
    self.cpu:add_mappings(range(0xD000, 0xDFFF), read_fixed_prg, select_chr(3))
    self.cpu:add_mappings(range(0xE000, 0xEFFF), read_fixed_prg, select_chr(4))
    self.cpu:add_mappings(range(0xF000, 0xFFFF), read_fixed_prg, mirroring)
end

ROM.MAPPER_DB[0x09] = MMC2

local MMC4 = {}
MMC4._mt = { __index = MMC4 }
setmetatable(MMC4, { __index = ROM })
function MMC4:new(...)
    local args = { ... }
    table.insert(args, MMC4._mt)
    local rom = ROM:new(unpack(args))
    setmetatable(rom, MMC4._mt)
    return rom
end

function MMC4:reset()
    self.wrk     = nil
    self.prg_ram = UTILS.fill({}, 0x00, 1024 * 8) -- 8K PRG RAM
    self.cpu:add_mappings(range(0x6000, 0x7fff), UTILS.tGetter(self.prg_ram, 1 - 0x6000),
        UTILS.tSetter(self.prg_ram, 1 - 0x6000))

    self.chr_banks            = UTILS.flattenSplit(self.chr_banks, 1024 * 4)
    self.prg_banks            = UTILS.flattenSplit(self.prg_banks, 1024 * 16)

    self.chr_bank_idxs        = { { 1, 2 }, { 1, 1 } } -- idxs into chr_banks
    self.current_chr_banks    = { 1, 1 }               -- idxs into chr_bank_idxs[1] and [2]
    self.ppu.chr_peek         = function(addr)
        local masked = band(addr, 0xFFF)               -- addr within bank
        local page = band(rshift(addr, 12), 0x1) + 1   -- address page 1 or 2

        local latch = self.current_chr_banks[page]
        local bank_idx = self.chr_bank_idxs[page][latch]
        local bank = self.chr_banks[bank_idx]

        if band(addr, 0x1FF8) == 0x0FD8 then
            self.current_chr_banks[1] = 1
        elseif band(addr, 0x1FF8) == 0x0FE8 then
            self.current_chr_banks[1] = 2
        elseif band(addr, 0x1FF8) == 0x1FD8 then
            self.current_chr_banks[2] = 1
        elseif band(addr, 0x1FF8) == 0x1FE8 then
            self.current_chr_banks[2] = 2
        end

        return bank[masked + 1]
    end
    self.ppu.chr_bg_read      = self.ppu.chr_peek
    self.ppu.chr_sprite_read  = self.ppu.chr_peek
    local select_chr          = function(reg_num)
        local idxs_table = reg_num <= 2 and self.chr_bank_idxs[1] or self.chr_bank_idxs[2]
        local idx_in_idxs_table
        if reg_num > 2 then
            idx_in_idxs_table = reg_num - 2
        else
            idx_in_idxs_table = reg_num
        end
        return function(_i, v)
            idxs_table[idx_in_idxs_table] = (band(v, 0x1F) % #(self.chr_banks)) + 1
        end
    end

    local read_switchable_prg = function(i)
        return self.current_prg_bank[i - 0x8000 + 1]
    end
    local last_prg_bank       = self.prg_banks[#(self.prg_banks)]
    local read_fixed_prg      = function(i)
        local addr = i - 0xC000
        return last_prg_bank[addr + 1]
    end
    local write_prg           = function(_i, v)
        self.prg_bank_idx = band(v, 0xF) % #(self.prg_banks)
        self.current_prg_bank = self.prg_banks[self.prg_bank_idx + 1]
    end
    local mirroring           = function(_i, v)
        self.ppu:nametables(band(v, 0x01) == 0x01 and "horizontal" or "vertical")
    end
    self.cpu:add_mappings(range(0x8000, 0x9FFF), read_switchable_prg, CPU.UNDEFINED)
    self.cpu:add_mappings(range(0xA000, 0xAFFF), read_switchable_prg, write_prg)
    self.cpu:add_mappings(range(0xB000, 0xBFFF), read_switchable_prg, select_chr(1))
    self.cpu:add_mappings(range(0xC000, 0xCFFF), read_fixed_prg, select_chr(2))
    self.cpu:add_mappings(range(0xD000, 0xDFFF), read_fixed_prg, select_chr(3))
    self.cpu:add_mappings(range(0xE000, 0xEFFF), read_fixed_prg, select_chr(4))
    self.cpu:add_mappings(range(0xF000, 0xFFFF), read_fixed_prg, mirroring)
end

ROM.MAPPER_DB[0x0A] = MMC4
