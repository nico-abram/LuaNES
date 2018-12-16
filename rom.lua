local serpent = require("serpent")
ROM = {}
local ROM = ROM
ROM._mt = {__index = ROM}

ROM.MAPPER_DB = {
    [0x00] = ROM
}

local nthBitIsSet = UTILS.nthBitIsSet
local isDefined = UTILS.isDefined
local bind = UTILS.bind
local tSetter = UTILS.tSetter
local tGetter = UTILS.tGetter
local fill = UTILS.fill
local range = UTILS.range
local map = UTILS.map
local flat_map = UTILS.flat_map
local uniq = UTILS.uniq
local clear = UTILS.clear
local all = UTILS.all
local copy = UTILS.copy
local nthBitIsSetInt = UTILS.nthBitIsSetInt
local transpose = UTILS.transpose

function ROM:initialize(conf, cpu, ppu, basename, bytes, str)
    self.conf = conf or {}
    self.cpu =
        cpu or
        {
            add_mappings = function()
            end
        }
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
        prg_count, chr_count, wrk_count = self:parse_header({unpack(bytes, idx + 1, idx + header_size)}, str)
        idx = header_size
    end
    if #bytes < 0x4000 * prg_count then
        error "EOF in ROM bank data"
    end
    self.prg_banks = {}
    do
        local prg_bank_size = 0x4000
        for i = 1, prg_count do
            self.prg_banks[i] = copy(bytes, prg_bank_size, idx)
            idx = idx + prg_bank_size
        end
    end

    if #bytes < 0x2000 * chr_count then
        error "EOF in CHR bank data"
    end
    self.chr_banks = {}
    do
        local chr_bank_size = 0x2000
        for i = 1, chr_count do
            self.chr_banks[i] = copy(bytes, chr_bank_size, idx)
            idx = idx + chr_bank_size
        end
    end
    self.prg_ref = {}
    fill(self.prg_ref, 0, 0x10000)
    for i = 0x8000 + 1, 0x4000 + 0x8000 + 1 do
        self.prg_ref[i] = self.prg_banks[1][i - 0x8000]
    end
    for i = 0xc000 + 1, 0x4000 + 0xc000 + 1 do
        self.prg_ref[i] = self.prg_banks[#(self.prg_banks)][i - 0xc000]
    end
    --[[
    UTILS.print("rominit")
    UTILS.print(self.prg_ref[0x8000 + 1])
    UTILS.print(self.prg_banks[1][1])
--]]
    self.chr_ram = chr_count == 0 -- No CHR bank implies CHR-RAM (writable CHR bank)
    self.chr_ref = self.chr_ram and (fill({}, 0, 0x2000)) or copy(self.chr_banks[1])

    self.wrk_readable = wrk_count > 0
    self.wrk_writable = false
    self.wrk =
        wrk_count > 0 and
        map(
            range(0x6000, 0x7fff),
            function(n)
                return bit.rshift(n, 8)
            end
        ) or
        nil

    self:init()

    self.ppu.nametables = self.mirroring
    self.ppu.set_chr_mem(self.chr_ref, self.chr_ram)
    --[[
    UTILS.print(#(self.chr_ref))
    UTILS.print(#(self.chr_banks))
    UTILS.print(#(self.prg_ref))
    UTILS.print((self.chr_ref[1]))
    UTILS.print(#(self.chr_banks[1]))
    UTILS.print((self.prg_ref[1]))
    --]]
end

function ROM:init()
end

function ROM:reset()
    self.cpu:add_mappings(range(0x8000, 0xffff), tGetter(self.prg_ref), CPU.UNDEFINED)
end

function ROM:inspect()
    return ""
    --[[{}
        "Mapper: #{ self.mapper } (#{ self.class.to_s.split("::").last })",
        "PRG Banks: #{ self.prg_banks.size }",
        "CHR Banks: #{ self.chr_banks.size }",
        "Mirroring: #{ self.mirroring }",
    }.join("\n")]]
end

function ROM:peek_6000(addr)
    return self.wrk_readable and self.wrk[addr - 0x6000] or bit.rshift(addr, 8)
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
    local sav = self.basename + ".sav"
    --return unless File.readable?(sav)
    self.wrk.replace(sav.bytes)
    local inp = assert(io.open(sav, "rb"))
    self.wrk = serpent.load(inp:read("*all"))
    assert(inp:close())
end

function ROM:save_battery()
    if not self.battery then
        return
    end
    local sav = self.basename + ".sav"
    print("Saving: " .. sav)
    local out = assert(io.open(sav, "wb"))
    out:write(serpent.dump(self.wrk))
    assert(out:close())
end

function ROM:new(conf, cpu, ppu, basename, bytes, str)
    local rom = {}
    setmetatable(rom, ROM._mt)
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
    local mapper = bit.bor(bit.rshift(blob[7], 4), bit.band(blob[8], 0xf0))

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
    if str:sub(1, 4) ~= "NES\x1a" then
        error "Missing 'NES' constant in header"
    end
    if nthBitIsSet(buf[7], 2) then
        error "trainer not supported"
    end
    if nthBitIsSet(buf[8], 0) then
        error "VS cart not supported"
    end
    if nthBitIsSet(buf[10], 0) then
        error "PAL not supported"
    end

    local prg_banks = buf[5]
    local chr_banks = buf[6]
    self.mirroring = not nthBitIsSet(buf[7], 0) and "horizontal" or "vertical"
    if nthBitIsSet(buf[7], 3) then
        self.mirroring = "four_screen"
    end
    self.battery = nthBitIsSet(buf[7], 1)
    self.mapper = bit.bor(bit.rshift(buf[7], 4), bit.band(buf[8], 0xf0))
    local ram_banks = math.max(1, buf[9])

    return prg_banks, chr_banks, ram_banks
end

local UxROM = {}
UxROM._mt = {__index = UxROM}
setmetatable(UxROM, {__index = ROM})
function UxROM:new(...)
    local rom = ROM:new(unpack(...))
    setmetatable(rom, UxROM._mt)
    return rom
end
function UxROM:reset()
    self.cpu.add_mappings(range(0x8000, 0xffff), tGetter(self.prg_ref), bind(self.poke_8000, self))
end

function UxROM:poke_8000(_addr, data)
    local j = bit.band(data, 7)
    for i = 0x8000 + 1, 0x4000 do
        self.prg_ref[i] = self.prg_banks[j][i - 0x8000]
    end
end
ROM.MAPPER_DB[0x02] = UxROM

local CNROM = {}
CNROM._mt = {__index = CNROM}
setmetatable(CNROM, {__index = ROM})
function CNROM:new(...)
    local rom = ROM:new(unpack(...))
    setmetatable(rom, CNROM._mt)
    return rom
end
function CNROM:reset()
    self.cpu.add_mappings(
        range(0x8000, 0xffff),
        tGetter(self.prg_ref),
        self.chr_ram and bind(self.poke_8000, self) or CPU.UNDEFINED
    )
end

function CNROM:poke_8000(_addr, data)
    local j = bit.band(data, 3)
    self.chr_ref = {unpack(self.chr_banks[j])}
end
ROM.MAPPER_DB[0x03] = CNROM

local MMC1 = {}
MMC1._mt = {__index = MMC1}
setmetatable(MMC1, {__index = ROM})
function MMC1:new(...)
    local rom = ROM:new(unpack(...))
    setmetatable(rom, MMC1._mt)
    return rom
end

local NMT_MODE = {"first", "second", "vertical", "horizontal"}
local PRG_MODE = {"conseq", "conseq", "fix_first", "fix_last"}
local CHR_MODE = {"conseq", "noconseq"}

function MMC1:init()
    self.chr_mode = nil
    self.nmt_mode = nil
    self.prg_mode = nil
    self.chr_bank_1 = 0
    self.prg_bank = 0
    self.chr_bank_0 = 0
end

function MMC1:reset()
    self.shift = 0
    self.shift_count = 0

    self.chr_banks = self.chr_banks.flatten.each_slice(0x1000).to_a

    self.wrk_readable = true
    self.wrk_writable = true
    self.cpu.add_mappings(range(0x6000, 0x7fff), bind(self.peek_6000, self), bind(self.poke_6000, self))
    self.cpu.add_mappings(range(0x8000, 0xffff), tGetter(self.prg_ref), bind(self.poke_prg, self))

    self:update_nmt("horizontal")
    self:update_prg("fix_last", 0, 0)
    self:update_chr("conseq", 0, 0)
end

function MMC1:poke_prg(addr, val)
    if val[7] == 1 then
        self.shift = 0
        self.shift_count = 0
    else
        self.shift = self.shift and self.shift or bit.lshift(val[0], self.shift_count)
        self.shift_count = self.shift_count + 1
        if self.shift_count == 0x05 then
            local x = bit.band(bit.rshift(addr, 13), 0x3)
            if x == 0 then -- control
                local nmt_mode = NMT_MODE[bit.band(self.shift, 3)]
                local prg_mode = PRG_MODE[bit.band(bit.rshift(self.shift, 2), 3)]
                local chr_mode = CHR_MODE[bit.band(bit.rshift(self.shift, 4), 1)]
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
end

function MMC1:update_nmt(nmt_mode)
    if self.nmt_mode == nmt_mode then
        return
    end
    self.nmt_mode = nmt_mode
    self.ppu.nametables = self.nmt_mode
end

function MMC1:update_prg(prg_mode, prg_bank, chr_bank_0)
    if prg_mode == self.prg_mode and prg_bank == self.prg_bank and chr_bank_0 == self.chr_bank_0 then
        return
    end
    self.prg_mode, self.prg_bank, self.chr_bank_0 = prg_mode, prg_bank, chr_bank_0

    local high_bit = bit.band(chr_bank_0, bit.band(0x10, (self.prg_banks.size - 1)))
    local prg_bank_ex = bit.band(bit.bor(bit.band(self.prg_bank, 0x0f), high_bit), (self.prg_banks.size - 1))
    local lower
    local upper
    if self.prg_mode == "conseq" then
        lower = bit.band(prg_bank_ex, bit.bnot(1))
        upper = lower + 1
    elseif self.prg_mode == "fix_first" then
        --elseif self.prg_mode == "fix_last" then
        lower = 0
        upper = prg_bank_ex
    else
        lower = prg_bank_ex
        upper = bit.bor(bit.band(self.prg_banks.size - 1, 0x0f), high_bit)
    end
    for i = 0x8000 + 1, 0x8000 + 0x4000 + 1 do
        self.prg_ref[i] = self.prg_banks[lower][i - 0x8000]
    end
    for i = 0xc000 + 1, 0xc000 + 0x4000 + 1 do
        self.prg_ref[i] = self.prg_banks[upper][i - 0xc000]
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
        lower = bit.band(self.chr_bank_0, 0x1e)
        upper = lower + 1
    else
        lower = self.chr_bank_0
        upper = self.chr_bank_1
    end
    for i = 0x0000 + 1, 0x1000 + 1 do
        self.chr_ref[i] = self.prg_banks[lower][i]
    end
    for i = 0x1000 + 1, 0x1000 + 0x1000 + 1 do
        self.chr_ref[i] = self.prg_banks[upper][i - 0x1000]
    end
end
ROM.MAPPER_DB[0x01] = MMC1

local MMC3 = {}
MMC3._mt = {__index = MMC3}
setmetatable(MMC3, {__index = ROM})
function MMC3:new(...)
    local rom = ROM:new(unpack(...))
    setmetatable(rom, MMC3._mt)
    return rom
end

function MMC3:init(rev) -- rev = :A or :B or :C
    rev = rev or "B"
    self.persistant = rev ~= "A"

    self.prg_banks = self.prg_banks.flatten.each_slice(0x2000).to_a
    self.prg_bank_swap = false

    self.chr_banks = self.chr_banks.flatten.each_slice(0x0400).to_a
    self.chr_bank_mapping = fill({}, nil, 8)
    self.chr_bank_swap = false
end

function MMC3:reset()
    self.wrk_readable = true
    self.wrk_writable = false

    local poke_a000 = self.mirroring ~= "FourScreen" and bind(self.poke_a000, self) or CPU.UNDEFINED
    self.cpu.add_mappings(range(0x6000, 0x7fff), bind(self.peek_6000, self), bind(self.poke_6000, self))
    local g = tGetter(self.prg_ref)
    self.cpu.add_mappings(range(0x8000, 0x9fff, 2), g, bind(self.poke_8000, self))
    self.cpu.add_mappings(range(0x8001, 0x9fff, 2), g, bind(self.poke_8001, self))
    self.cpu.add_mappings(range(0xa000, 0xbfff, 2), g, poke_a000)
    self.cpu.add_mappings(range(0xa001, 0xbfff, 2), g, bind(self.poke_a001, self))
    self.cpu.add_mappings(range(0xc000, 0xdfff, 2), g, bind(self.poke_c000, self))
    self.cpu.add_mappings(range(0xc001, 0xdfff, 2), g, bind(self.poke_c001, self))
    self.cpu.add_mappings(range(0xe000, 0xffff, 2), g, bind(self.poke_e000, self))
    self.cpu.add_mappings(range(0xe001, 0xffff, 2), g, bind(self.poke_e001, self))

    self:update_prg(0x8000, 0)
    self:update_prg(0xa000, 1)
    self:update_prg(0xc000, -2)
    self:update_prg(0xe000, -1)
    for i = 0, 7 do
        self:update_chr(i * 0x400, i)
    end
    --8.times {|i| update_chr(i * 0x400, i) }

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
    bank = bank % self.prg_banks.size
    if self.prg_bank_swap and addr[13] == 0 then
        addr = addr ^ 0x4000
    end
    for i = addr + 1, addr + 0x2000 + 1 do
        self.prg_ref[i] = self.prg_banks[bank][i - addr]
    end
end

function MMC3:update_chr(addr, bank)
    if self.chr_ram then
        return
    end
    local idx = addr / 0x400
    bank = bank % self.chr_banks.size
    if self.chr_bank_mapping[idx] == bank then
        return
    end
    if self.chr_bank_swap then
        addr = addr ^ 0x1000
    end
    self.ppu:update(0)
    for i = addr + 1, addr + 0x400 + 1 do
        self.chr_ref[i] = self.chr_banks[bank][i - addr]
    end
    self.chr_bank_mapping[idx] = bank
end

function MMC3:poke_8000(_addr, data)
    self.reg_select = bit.band(data, 7)
    local prg_bank_swap = data[6] == 1
    local chr_bank_swap = data[7] == 1

    if prg_bank_swap ~= self.prg_bank_swap then
        self.prg_bank_swap = prg_bank_swap
        for i = 0x8000 + 1, 0x8000 + 0x2000 + 1 do
            self.prg_ref[i] = self.prg_ref[bank][i - 0x8000 + 0xc000]
        end
        for i = 0xc000 + 1, 0xc000 + 0x2000 + 1 do
            self.chr_ref[i] = self.prg_ref[bank][i - 0xc000 + 0x8000]
        end
    --self.prg_ref[0x8000, 0x2000], self.prg_ref[0xc000, 0x2000] = self.prg_ref[0xc000, 0x2000], self.prg_ref[0x8000, 0x2000]
    end

    if chr_bank_swap ~= self.chr_bank_swap then
        self.chr_bank_swap = chr_bank_swap
        if not self.chr_ram then
            self.ppu.update(0)
            self.chr_ref = UTILS.rotate(self.chr_ref, 0x1000)
            self.chr_bank_mapping = UTILS.rotate(self.chr_bank_mapping, 4)
        end
    end
end

function MMC3:poke_8001(_addr, data)
    if self.reg_select < 6 then
        if self.reg_select < 2 then
            self:update_chr(self.reg_select * 0x0800, bit.band(data, 0xfe))
            self:update_chr(self.reg_select * 0x0800 + 0x0400, bit.bor(data, 0x01))
        else
            self:update_chr((self.reg_select - 2) * 0x0400 + 0x1000, data)
        end
    else
        self:update_prg((self.reg_select - 6) * 0x2000 + 0x8000, bit.band(data, 0x3f))
    end
end

function MMC3:poke_a000(_addr, data)
    self.ppu.nametables = data[0] == 1 and "horizontal" or "vertical"
end

function MMC3:poke_a001(_addr, data)
    self.wrk_readable = data[7] == 1
    self.wrk_writable = data[6] == 0 and self.wrk_readable
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
    self.cpu.clear_irq(CPU.IRQ_EXT)
end

function MMC3:poke_e001(_addr, _data)
    self.ppu:update(0)
    self.enabled = true
end

function MMC3:vsync()
    self.clock = self.clock > self.cpu.next_frame_clock and self.clock - self.cpu.next_frame_clock or 0
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
