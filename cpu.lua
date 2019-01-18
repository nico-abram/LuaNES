local UTILS = UTILS
UTILS:import()

CPU = {
    RP2A03_CC = 12,
    FOREVER_CLOCK = 0xffffffff
}
local CPU = CPU
CPU._mt = {__index = CPU}

do
    CPU.CLK = {}
    local clocks = {1, 2, 3, 4, 5, 6, 7, 8}
    for i = 1, #clocks do
        CPU.CLK[i] = clocks[i] * CPU.RP2A03_CC
    end
end
CPU.UNDEFINED = {}
CPU.RAM_SIZE = 0x0800
CPU.MAINMEM_SIZE = 0x10000
CPU.NMI_VECTOR = 0xfffa
CPU.RESET_VECTOR = 0xfffc
CPU.IRQ_VECTOR = 0xfffe

CPU.IRQ_EXT = 0x01
CPU.IRQ_FRAME = 0x40
CPU.IRQ_DMC = 0x80

CPU.NMIVector = 0xFFFA
CPU.ResetVector = 0xFFFC
CPU.IRQVector = 0xFFFE
CPU.ClockRates = {
    Ntsc = 1789773,
    Pal = 1662607,
    Dendy = 1773448
}

local UNDEFINED = CPU.UNDEFINED
local CLK = CPU.CLK

local isDefined = UTILS.isDefined
local bind = UTILS.bind
local tSetter = UTILS.tSetter
local tGetter = UTILS.tGetter
local fill = UTILS.fill
local range = UTILS.range
local printf = UTILS.printf
local nthBitIsSetInt = UTILS.nthBitIsSetInt

CPU.PokeNop = function()
end

function CPU:steal_clocks(clk)
    self.clk = self.clk + clk
end

function CPU:odd_clock()
    --print "odd_clock"
    --print(self.clk_total)
    --print(self.clk)
    --print(CLK[2])
    return ((self.clk_total + self.clk) % CLK[2]) ~= 0
end

function CPU:update()
    --print "cpu update"
    --print(self.clk)
    self.apu:clock_dma(self.clk)
    --print(self.clk)
    return self.clk
end

function CPU:dmc_dma(addr)
    -- This is inaccurate; it must steal *up to* 4 clocks depending upon
    -- whether CPU writes in this clock, but this always steals 4 clocks.
    self.clk = self.clk + CLK[3]
    local dma_buffer = self:fetch(addr)
    self.clk = self.clk + CLK[1]
    return dma_buffer
end

function CPU:next_frame_clock()
    return self.clk_frame
end

function CPU:set_next_frame_clock(x)
    self.clk_frame = x
    self.clk_target = x < self.clk_target and x or self.clk_target
    --[[
    printf("set_next_frame_clock")
    printf("%04X", x)
    printf("%04X", self.clk_target)
    ]]
    return x
end
function CPU:current_clock()
    return self.clk
end

function CPU:peek_nop(addr)
    --printf("PNOP:%02X",addr)
    return rshift(addr, 8)
end

function CPU:peek_jam_1(addr)
    self._pc = band(self._pc - 1, 0xffff)
    return 0xfc
end
function CPU:peek_jam_2(_addr)
    return 0xff
end

function CPU:peek_ram(addr)
    return self.ram[addr % CPU.RAM_SIZE]
end
function CPU:poke_ram(addr, data)
    self.ram[addr % CPU.RAM_SIZE] = data
end

-- read an addr (8 bit)
function CPU:fetch(addr)
    local getter = self._fetch[addr]
    getter = (type(getter) ~= "table" or getter ~= UNDEFINED) and getter or nil
    --[[
    if not getter then
        printf("Error fetching %04X state:", addr)
        self:printState()
        local reach = 2
        printf("Surroundings (range:%i):", reach)
        for i = addr - reach, addr + reach do
            local getter2 = self._fetch[i]
            if getter2 and (type(getter2) ~= "table" or getter2 ~= UNDEFINED) then
                printf("%04X %04x", i, getter2(i))
            else
                if not getter2 then
                    printf("%04X NIL", i)
                else
                    printf("%04X UNDEF", i)
                end
            end
        end
        error(string.format("Error fetching %04X", addr))
    end
    --]]
    --print "ASD"
    --printf("%04X : %04X", addr, getter(addr))
    return getter and getter(addr) or nil
end
function CPU:store(addr, value)
    --print(type(self._store[addr])=="table" and self._store[addr]==UNDEFINED)
    return self._store[addr](addr, value)
end

-- Read 16 bits (word) from addr
function CPU:peek16(addr)
    --print "peek16"
    local a = self:fetch(addr)
    local b = lshift(self:fetch(addr + 1), 8)
    --print(a)
    --print(b)
    local x = a + b
    --[[
    print "peek16"
printf("%02X", a)
  printf("%02X", b)
    UTILS.print(a)
    UTILS.print(b)
    UTILS.print(addr)
    UTILS.print(x)
    --]]
    return x
end

function CPU:add_mappings(addr, peek, poke)
    self.peeks[peek] = isDefined(self.peeks[peek]) and self.peeks[peek] or peek
    peek = self.peeks[peek]
    self.pokes[poke] = isDefined(self.pokes[poke]) and self.pokes[poke] or poke
    poke = self.pokes[poke]
    if type(addr) == "number" then
        addr = {addr}
    end
    for i = addr[0] and 0 or 1, #addr do
        local x = addr[i]
        self._fetch[x] = peek
        if poke and type(poke) ~= "table" then
            self._store[x] = poke
        else
            self._store[x] = CPU.PokeNop
        end
    end
end

function CPU:reset()
    self._a = 0
    self._x = 0
    self._y = 0
    self._sp = 0xfd
    self._pc = 0xfffc
    self._p_nz = 1
    self._p_c = 0
    self._p_v = 0
    self._p_i = 0x04
    self._p_d = 0
    fill(self.ram, 0xff)
    self.clk = 0
    self.clk_total = 0
    self:add_mappings(range(0x0000, 0x07ff), tGetter(self.ram, 0), tSetter(self.ram))
    self:add_mappings(range(0x0800, 0x1fff), bind(self.peek_ram, self), bind(self.poke_ram, self))
    self:add_mappings(range(0x2000, 0xffff), bind(self.peek_nop, self), UNDEFINED) -- 8 PPU registers mirrored
    --self:add_mappings(0xfffc, bind(self.peek_jam_1, self), UNDEFINED)
    --self:add_mappings(0xfffd, bind(self.peek_jam_2, self), UNDEFINED)
end

function CPU:setpc()
    --[[
      print "RERERERE"
      printf("%04X",self:fetch(CPU.RESET_VECTOR-1))
      print "RERERERE"
      printf("%04X",self:fetch(CPU.RESET_VECTOR))
      print "RERERERE"
      printf("%04X",self:fetch(CPU.RESET_VECTOR+1))
      print "RERERERE"
      printf("%04X",self._pc)
      print "RERERERE"
      printf("%04X",self:peek16(CPU.RESET_VECTOR-1))
      print "RERERERE"
      printf("%04X",self:peek16(CPU.RESET_VECTOR+1))
      print "RERERERE"
      printf("%04X",self:peek16(CPU.RESET_VECTOR+2))
      ]]
end

function CPU:new(conf)
    local cpu = {}
    setmetatable(cpu, CPU._mt)
    cpu.conf = conf or {loglevel = 0, pc = nil}
    cpu.ram = fill({}, CPU.UNDEFINED, CPU.RAM_SIZE)
    cpu._store = fill({}, CPU.UNDEFINED, CPU.MAINMEM_SIZE)
    cpu._fetch = fill({}, CPU.UNDEFINED, CPU.MAINMEM_SIZE)
    cpu.peeks = {}
    cpu.pokes = {}
    cpu.clk_rate = CPU.ClockRates.Ntsc
    cpu.clk = 0 -- the current clock
    cpu.clk_frame = 0
    cpu.clk_target = 0 -- the goal clock for the current CPU#run
    cpu.clk_total = 0 -- the total elapsed clocks
    cpu.clk_nmi = CPU.FOREVER_CLOCK -- the next NMI clock (CPU.FOREVER_CLOCK means "not scheduled")
    cpu.clk_irq = CPU.FOREVER_CLOCK -- the next IRQ clock
    cpu.irq_flags = 0
    cpu.jammed = false
    cpu:reset()
    cpu.data = 0
    cpu.addr = 0
    cpu.opcode = nil
    cpu.ppu_sync = nil
    -- DUMMY APU
    cpu.apu = {
        clock_dma = function(clk)
        end,
        do_clock = function()
            return CLK[1]
        end
    }
    return cpu
end

function CPU:dmc_dma(addr)
    -- This is inaccurate; it must steal *up to* 4 clocks depending upon
    -- whether CPU writes in this clock, but this always steals 4 clocks.
    self.clk = CLK[3] + self.clk
    local dma_buffer = self:fetch(addr)
    self.clk = CLK[1] + self.clk
    return dma_buffer
end

function CPU:sprite_dma(addr, sp_ram)
    for i = 1, 256 do
        sp_ram[i] = self.ram[addr + i - 1]
    end
    for i = 0, 63 do
        local j = i * 4 + 3
        sp_ram[j] = band(sp_ram[j], 0xe3)
    end
end

function CPU:boot()
    self.clk = CLK[7] -- clocks it takes to boot
    --self._pc = 0xC000
    --print "boot"
    --printf("%04X", self._pc)
    self._pc = self.conf.pc or self:peek16(CPU.RESET_VECTOR)
    --printf("%04X", self._pc)
end

function CPU:vsync()
    if self.ppu_sync then
        (self.ppu):sync(self.clk)
    end

    self.clk = self.clk - self.clk_frame
    self.clk_total = self.clk_total + self.clk_frame

    if self.clk_nmi ~= CPU.FOREVER_CLOCK then
        self.clk_nmi = self.clk_nmi - self.clk_frame
    end
    if self.clk_irq ~= CPU.FOREVER_CLOCK then
        self.clk_irq = self.clk_irq - self.clk_frame
    end
    if self.clk_irq < 0 then
        self.clk_irq = 0
    end
end

-------------------------------------------------------------------------------------------------------------------
-- interrupts

function CPU:clear_irq(line)
    local old_irq_flags = band(self.irq_flags, bor(CPU.IRQ_FRAME, CPU.IRQ_DMC))
    self.irq_flags = band(bxor(self.irq_flags, bxor(line, bor(bor(CPU.IRQ_EXT, CPU.IRQ_FRAME), CPU.IRQ_DMC))))
    if self.irq_flags == 0 then
        self.clk_irq = CPU.FOREVER_CLOCK
    end
    return old_irq_flags
end

function CPU:next_interrupt_clock(clk)
    clk = clk + CLK[1] + CLK[1] / 2 -- interrupt edge
    if self.clk_target > clk then
        self.clk_target = clk
    end
    return clk
end

function CPU:do_irq(line, clk)
    self.irq_flags = bor(self.irq_flags, line)
    if self.clk_irq == CPU.FOREVER_CLOCK and self._p_i == 0 then
        self.clk_irq = self:next_interrupt_clock(clk)
    end
end

function CPU:do_nmi(clk)
    if self.clk_nmi == CPU.FOREVER_CLOCK then
        self.clk_nmi = self:next_interrupt_clock(clk)
    end
end

function CPU:do_isr(vector)
    if self.jammed then
        return
    end
    self:push16(self._pc)
    self:push8(self:flags_pack())
    self._p_i = 0x04
    self.clk = self.clk + CLK[7]
    local addr = vector == CPU.NMI_VECTOR and CPU.NMI_VECTOR or self:fetch_irq_isr_vector()
    self._pc = self:peek16(addr)
end

function CPU:fetch_irq_isr_vector()
    if self.clk >= self.clk_frame then
        --print("30000")
        self:fetch(0x3000)
    end
    if self.clk_nmi ~= CPU.FOREVER_CLOCK then
        if self.clk_nmi + CLK[2] <= self.clk then
            self.clk_nmi = CPU.FOREVER_CLOCK
            --print("NMI")
            return CPU.NMI_VECTOR
        end
        self.clk_nmi = self.clk + 1
    end
    --print("SAD")
    return CPU.IRQ_VECTOR
end

------------------------------------------------------------------------------------------------------------------------
-- instruction helpers

------ P regeister ------

function CPU:flags_pack()
    return bor(
        bor(
            bor(
                bor(
                    bor(
                        bor(
                            band(bor(rshift(self._p_nz, 1), self._p_nz), 0x80),
                            (band(self._p_nz, 0xff) ~= 0 and 0 or 2)
                        ),
                        self._p_c
                    ),
                    self._p_v ~= 0 and 0x40 or 0
                ),
                self._p_i
            ),
            self._p_d
        ),
        0x20
    )
    --[[
        -- NVssDIZC
      ((self._p_nz | self._p_nz >> 1) & 0x80) | -- N: Negative
        (self._p_nz & 0xff ~= 0 ? 0 : 2) |  -- Z: Zero
        self._p_c |                         -- C: Carry
        (self._p_v ~= 0 ? 0x40 : 0) |       -- V: Overflow
        self._p_i |                         -- I: Inerrupt
        self._p_d |                         -- D: Decimal
        0x20
        --]]
end

function CPU:flags_unpack(f)
    self._p_nz = bor(band(bnot(f), 2), lshift(band(f, 0x80), 1))
    self._p_c = band(f, 0x01)
    self._p_v = band(f, 0x40)
    self._p_i = band(f, 0x04)
    self._p_d = band(f, 0x08)
end

------ branch helper ------
function CPU:branch(cond)
    if cond then
        local tmp = self._pc + 1
        local rel = self:fetch(self._pc)
        self._pc = band(tmp + (rel < 128 and rel or bor(rel, 0xff00)), 0xffff)
        self.clk = self.clk + (nthBitIsSetInt(tmp, 8) == nthBitIsSetInt(self._pc, 8) and CLK[3] or CLK[4])
    else
        self._pc = self._pc + 1
        self.clk = self.clk + CLK[2]
    end
end

------ storers ------
function CPU:store_mem()
    --print(self.clk)
    self:store(self.addr, self.data)
    --print(self.clk)
    self.clk = self.clk + CLK[1]
    --print(self.clk)
end

function CPU:store_zpg()
    self.ram[self.addr] = self.data
end

------ stack management ------
function CPU:push8(data)
    self.ram[0x0100 + self._sp] = data
    self._sp = band((self._sp - 1), 0xff)
    --printf("push8")
    --printf("%X",self._sp)
end

function CPU:push16(data)
    self:push8(rshift(data, 8))
    self:push8(band(data, 0xff))
end

function CPU:pull8()
    --printf("pull8")
    --printf("%X",self._sp)
    self._sp = band(self._sp + 1, 0xff)
    --printf("%X",self._sp)
    --printf("%02X",0x0100 + self._sp)
    return self.ram[0x0100 + self._sp]
end

function CPU:pull16()
    local x = self:pull8()
    local b = self:pull8()
    --[[
    print("pull")
      printf("%X",x)
      printf("%X",b)
      ]]
    return x + 256 * b
end

------------------------------------------------------------------------------------------------------------------------
-- addressing modes

-- immediate addressing (read only)
function CPU:imm(_read, _write)
    self.data = self:fetch(self._pc)
    --printf( "%02X" ,self._pc)
    self._pc = self._pc + 1
    self.clk = self.clk + CLK[2]
end

-- zero-page addressing
function CPU:zpg(read, write)
    self.addr = self:fetch(self._pc)
    self._pc = self._pc + 1
    self.clk = self.clk + CLK[3]
    if read then
        self.data = self.ram[self.addr]
        if write then
            self.clk = self.clk + CLK[2]
        end
    end
end

-- zero-page indexed addressing
function CPU:zpg_reg(indexed, read, write)
    self.addr = band(indexed + self:fetch(self._pc), 0xff)
    self._pc = self._pc + 1
    self.clk = self.clk + CLK[4]
    if read then
        self.data = self.ram[self.addr]
        if write then
            self.clk = self.clk + CLK[2]
        end
    end
end

function CPU:zpg_x(read, write)
    return self:zpg_reg(self._x, read, write)
end

function CPU:zpg_y(read, write)
    return self:zpg_reg(self._y, read, write)
end
-- absolute addressing
function CPU:abs(read, write)
    self.addr = self:peek16(self._pc)
    self._pc = self._pc + 2
    --print(self.clk)
    self.clk = self.clk + CLK[3]
    --print(self.clk)
    return self:read_write(read, write)
end

-- absolute indexed addressing
function CPU:abs_reg(indexed, read, write)
    local addr = self._pc + 1
    --printf("pc:%02X", self._pc)
    local i = indexed + self:fetch(self._pc)
    --[[
  printf("addr:%02X", self.addr)
  printf("idxd:%02X", indexed)
  printf("postpc:%02X", self._pc)
  printf("i:%02X", i)
  printf("laddr:%02X", addr)
  ]]
    self.addr = band(lshift(self:fetch(addr), 8) + i, 0xffff)
    --printf("postaddr%02X", self.addr)
    if write then
        addr = band(self.addr - band(i, 0x100), 0xffff)
        self:fetch(addr)
        self.clk = self.clk + CLK[4]
    else
        self.clk = self.clk + CLK[3]
        if band(i, 0x100) ~= 0 then
            addr = band(self.addr - 0x100, 0xffff) -- for inlining fetch
            self:fetch(addr)
            self.clk = self.clk + CLK[1]
        end
    end
    --printf("%02X", addr)
    self:read_write(read, write)
    self._pc = self._pc + 2
end

function CPU:abs_x(read, write)
    return self:abs_reg(self._x, read, write)
end

function CPU:abs_y(read, write)
    return self:abs_reg(self._y, read, write)
end
-- indexed indirect addressing
function CPU:ind_x(read, write)
    local addr = self:fetch(self._pc) + self._x
    self._pc = self._pc + 1
    self.clk = self.clk + CLK[5]
    self.addr = bor(self.ram[band(addr, 0xff)], lshift(self.ram[band(addr + 1, 0xff)], 8))
    return self:read_write(read, write)
end

-- indirect indexed addressing
function CPU:ind_y(read, write)
    local addr = self:fetch(self._pc)
    self._pc = self._pc + 1
    local indexed = self.ram[addr] + self._y
    self.clk = self.clk + CLK[4]
    if write then
        self.clk = self.clk + CLK[1]
        self.addr = lshift(self.ram[band(addr + 1, 0xff)], 8) + indexed
        addr = self.addr - band(indexed, 0x100) -- for inlining fetch
        self:fetch(addr)
    else
        self.addr = band(lshift(self.ram[band(addr + 1, 0xff)], 8) + indexed, 0xffff)
        if band(indexed, 0x100) ~= 0 then
            addr = band(self.addr - 0x100, 0xffff) -- for inlining fetch
            self:fetch(addr)
            self.clk = self.clk + CLK[1]
        end
    end
    return self:read_write(read, write)
end

function CPU:read_write(read, write)
    if read then
        --print(string.format("%04X",self.addr))
        self.data = self:fetch(self.addr)
        --print(self.clk)
        self.clk = self.clk + CLK[1]
        --print(self.clk)
        if write then
            self:store(self.addr, self.data)
            --print(self.clk)
            self.clk = self.clk + CLK[1]
        --print(self.clk)
        end
    end
end

--------------------------------------------------------------------------------------------------------------------
-- instructions

-- load instructions
function CPU:_lda()
    --printf("%02X",self.data)
    self._p_nz = self.data
    self._a = self.data
end

function CPU:_ldx()
    --printf("%02X",self.data)
    self._p_nz = self.data
    self._x = self.data
    --printf("%02X",self._x)
    --printf("%02X",self._p_nz)
end

function CPU:_ldy()
    self._y = self.data
    self._p_nz = self.data
end

-- store instructions
function CPU:_sta()
    self.data = self._a
end

function CPU:_stx()
    --print(self._x)
    --print(self.data)
    self.data = self._x
    --printf("%02X",self.ram[0x07ff])
end

function CPU:_sty()
    self.data = self._y
end

-- transfer instructions
function CPU:_tax()
    self.clk = self.clk + CLK[2]
    self._x = self._a
    self._p_nz = self._a
end

function CPU:_tay()
    self.clk = self.clk + CLK[2]
    self._y = self._a
    self._p_nz = self._a
end

function CPU:_txa()
    self.clk = self.clk + CLK[2]
    self._a = self._x
    self._p_nz = self._x
end

function CPU:_tya()
    self.clk = self.clk + CLK[2]
    self._a = self._y
    self._p_nz = self._y
end

-- flow control instructions
function CPU:_jmp_a()
    self._pc = self:peek16(self._pc)
    self.clk = self.clk + CLK[3]
end

function CPU:_jmp_i()
    local pos = self:peek16(self._pc)
    local low = self:fetch(pos)
    pos = bor(band(pos, 0xff00), band(pos + 1, 0x00ff))
    local high = self:fetch(pos)
    self._pc = high * 256 + low
    self.clk = self.clk + CLK[5]
end

function CPU:_jsr()
    local data = self._pc + 1
    self:push16(data)
    self._pc = self:peek16(self._pc)
    self.clk = self.clk + CLK[6]
end

function CPU:_rts()
    local x = self:pull16()
    --printf("rts")
    --printf("%X04",x)
    self._pc = band(x + 1, 0xffff)
    self.clk = self.clk + CLK[6]
end

function CPU:_rti()
    self.clk = self.clk + CLK[6]
    local packed = self:pull8()
    self._pc = self:pull16()
    self:flags_unpack(packed)
    if self.irq_flags == 0 or self._p_i ~= 0 then
        self.clk_irq = CPU.FOREVER_CLOCK
    else
        self.clk_target = 0
        self.clk_irq = 0
    end
end

function CPU:_bne()
    return self:branch(band(self._p_nz, 0xff) ~= 0)
end

function CPU:_beq()
    return self:branch(band(self._p_nz, 0xff) == 0)
end

function CPU:_bmi()
    return self:branch(band(self._p_nz, 0x180) ~= 0)
end

function CPU:_bpl()
    return self:branch(band(self._p_nz, 0x180) == 0)
end

function CPU:_bcs()
    return self:branch(self._p_c ~= 0)
end

function CPU:_bcc()
    return self:branch(self._p_c == 0)
end

function CPU:_bvs()
    return self:branch(self._p_v ~= 0)
end

function CPU:_bvc()
    return self:branch(self._p_v == 0)
end

-- math operations
function CPU:_adc()
    local tmp = self._a + self.data + self._p_c
    self._p_v = band(bnot(bxor(self._a, self.data)), band(bxor(self._a, tmp), 0x80))
    self._a = band(tmp, 0xff)
    self._p_nz = self._a
    self._p_c = nthBitIsSetInt(tmp, 8)
end

function CPU:_sbc()
    local data = bxor(self.data, 0xff)
    local tmp = self._a + data + self._p_c
    self._p_v = band(bnot(bxor(self._a, data)), band(bxor(self._a, tmp), 0x80))
    self._a = band(tmp, 0xff)
    self._p_nz = self._a
    self._p_c = nthBitIsSetInt(tmp, 8)
end

-- logical operations
function CPU:_and()
    self._a = band(self._a, self.data)
    self._p_nz = self._a
end

function CPU:_ora()
    self._a = bor(self._a, self.data)
    self._p_nz = self._a
end

function CPU:_eor()
    self._a = bxor(self._a, self.data)
    self._p_nz = self._a
end

function CPU:_bit()
    self._p_nz = bor((band(self.data, self._a) ~= 0 and 1 or 0), lshift(band(self.data, 0x80), 1))
    self._p_v = band(self.data, 0x40)
end

function CPU:_cmp()
    local data = self._a - self.data
    self._p_nz = band(data, 0xff)
    self._p_c = 1 - nthBitIsSetInt(data, 8)
end

function CPU:_cpx()
    local data = self._x - self.data
    self._p_nz = band(data, 0xff)
    self._p_c = 1 - nthBitIsSetInt(data, 8)
end

function CPU:_cpy()
    local data = self._y - self.data
    self._p_nz = band(data, 0xff)
    self._p_c = 1 - nthBitIsSetInt(data, 8)
end

-- shift operations
function CPU:_asl()
    self._p_c = rshift(self.data, 7)
    self._p_nz = band(lshift(self.data, 1), 0xff)
    self.data = self._p_nz
end

function CPU:_lsr()
    self._p_c = band(self.data, 1)
    self._p_nz = rshift(self.data, 1)
    self.data = self._p_nz
end

function CPU:_rol()
    self._p_nz = bor(band(lshift(self.data, 1), 0xff), self._p_c)
    self._p_c = rshift(self.data, 7)
    self.data = self._p_nz
end

function CPU:_ror()
    self._p_nz = bor(rshift(self.data, 1), lshift(self._p_c, 7))
    self._p_c = band(self.data, 1)
    self.data = self._p_nz
end

-- increment and decrement operations
function CPU:_dec()
    self._p_nz = band(self.data - 1, 0xff)
    self.data = self._p_nz
end

function CPU:_inc()
    self._p_nz = band(self.data + 1, 0xff)
    self.data = self._p_nz
end

function CPU:_dex()
    self.clk = self.clk + CLK[2]
    local x = band(self._x - 1, 0xff)
    self.data = x
    self._p_nz = x
    self._x = x
end

function CPU:_dey()
    self.clk = self.clk + CLK[2]
    self._y = band(self._y - 1, 0xff)
    self.data = self._y
    self._p_nz = self._y
end

function CPU:_inx()
    self.clk = self.clk + CLK[2]
    self._x = band(self._x + 1, 0xff)
    self.data = self._x
    self._p_nz = self._x
end

function CPU:_iny()
    self.clk = self.clk + CLK[2]
    self._y = band(self._y + 1, 0xff)
    self.data = self._y
    self._p_nz = self._y
end

-- flags instructions
function CPU:_clc()
    self.clk = self.clk + CLK[2]
    self._p_c = 0
end

function CPU:_sec()
    self.clk = self.clk + CLK[2]
    self._p_c = 1
end

function CPU:_cld()
    self.clk = CLK[2] + self.clk
    self._p_d = 0
end

function CPU:_sed()
    self.clk = self.clk + CLK[2]
    self._p_d = 8
end

function CPU:_clv()
    self.clk = self.clk + CLK[2]
    self._p_v = 0
end
function CPU:_sei()
    self.clk = self.clk + CLK[2]
    if self._p_i == 0 then
        self._p_i = 0x04
        self.clk_irq = CPU.FOREVER_CLOCK
        if self.irq_flags ~= 0 then
            self:do_isr(CPU.IRQ_VECTOR)
        end
    end
end

function CPU:_cli()
    self.clk = self.clk + CLK[2]
    if self._p_i ~= 0 then
        self._p_i = 0
        if self.irq_flags ~= 0 then
            local clk = self.clk + 1
            self.clk_irq = clk
            if self.clk_target > clk then
                self.clk_target = clk
            end
        end
    end
end

-- stack operations
function CPU:_pha()
    self.clk = self.clk + CLK[3]
    return self:push8(self._a)
end

function CPU:_php()
    self.clk = self.clk + CLK[3]
    local data = bor(self:flags_pack(), 0x10)
    return self:push8(data)
end

function CPU:_pla()
    self.clk = self.clk + CLK[4]
    self._a = self:pull8()
    self._p_nz = self._a
end

function CPU:_plp()
    self.clk = self.clk + CLK[4]
    local i = self._p_i
    self:flags_unpack(self:pull8())
    if self.irq_flags ~= 0 then
        if i > self._p_i then
            local clk = self.clk + 1
            self.clk_irq = clk
            if self.clk_target > clk then
                self.clk_target = clk
            end
        elseif i < self._p_i then
            self.clk_irq = CPU.FOREVER_CLOCK
            self:do_isr(CPU.IRQ_VECTOR)
        end
    end
end

function CPU:_tsx()
    self.clk = self.clk + CLK[2]
    self._p_nz = self._sp
    self._x = self._sp
end

function CPU:_txs()
    self.clk = self.clk + CLK[2]
    self._sp = self._x
    --printf("txs")
    --printf("%X04",self._sp)
end

-- undocumented instructions, rarely used
function CPU:_anc()
    self._a = band(self._a, self.data)
    self._p_nz = self._a
    self._p_c = rshift(self._p_nz, 7)
end

function CPU:_ane()
    self._a = band(band(bor(self._a, 0xee), self._x), self.data)
    self._p_nz = self._a
end

function CPU:_arr()
    self._a = bor(rshift(band(self.data, self._a), 1), lshift(self._p_c, 7))
    self._p_nz = self._a
    self._p_c = nthBitIsSetInt(self._a, 6)
    self._p_v = bxor(nthBitIsSetInt(self._a, 6), nthBitIsSetInt(self._a, 5))
end

function CPU:_asr()
    self._p_c = band(band(self.data, self._a), 0x1)
    self._a = rshift(band(self.data, self._a), 1)
    self._p_nz = self._a
end

function CPU:_dcp()
    self.data = band(self.data - 1, 0xff)
    return self:_cmp()
end

function CPU:_isb()
    self.data = band(self.data + 1, 0xff)
    return self:_sbc()
end

function CPU:_las()
    self._sp = band(self._sp, self.data)
    --printf("las")
    --printf("%X04",self._sp)
    self._x = self._sp
    self._p_nz = self._x
    self._a = self._x
end

function CPU:_lax()
    self._x = self.data
    self._p_nz = self._x
    self._a = self._x
end

function CPU:_lxa()
    self._x = self.data
    self._p_nz = self._x
    self._a = self._x
end

function CPU:_rla()
    local c = self._p_c
    self._p_c = rshift(self.data, 7)
    self.data = bor(band(lshift(self.data, 1), 0xff), c)
    self._a = band(self._a, self.data)
    self._p_nz = self._a
end

function CPU:_rra()
    --[[
      local p = 0x647
      local ad = self.addr
      local pc = self._p_c
      self.addr = p
      local c = lshift(self._p_c ,7)
      self._p_c = band(self.data , 1)
      self.data = bor(rshift(self.data ,1), c)
      printf("%02X", self.addr)
      printf("%02X", self:fetch(self.addr))
      printf("%02X", c)
      printf("%02X", self.data)
      printf("%02X", self._p_c)
      printf("%02X", self.data)
      self.addr = ad
      self._p_c = p
      ]]
    local c = lshift(self._p_c, 7)
    self._p_c = band(self.data, 1)
    self.data = bor(rshift(self.data, 1), c)
    --[[
      printf("%02X", self.addr)
      printf("%02X", self.addr)
      printf("%02X", self:fetch(self.addr))
      printf("%02X", c)
      printf("%02X", self.data)
      printf("%02X", self._p_c)
      printf("%02X", self.data)
      ]]
    return self:_adc()
end

function CPU:_sax()
    self.data = band(self._a, self._x)
end

function CPU:_sbx()
    self.data = band(self._a, self._x) - self.data
    self._p_c = band(self.data, 0xffff) <= 0xff and 1 or 0
    self._x = band(self.data, 0xff)
    self._p_nz = self._x
end

function CPU:_sha()
    self.data = band(self._a, band(self._x, (rshift(self.addr, 8) + 1)))
end

function CPU:_shs()
    self._sp = band(self._a, self._x)
    --printf("shs")
    --printf("%X04",self._sp)
    self.data = band(self._sp, (rshift(self.addr, 8) + 1))
end

function CPU:_shx()
    self.data = band(self._x, (rshift(self.addr, 8) + 1))
    self.addr = bor(lshift(self.data, 8), band(self.addr, 0xff))
end

function CPU:_shy()
    self.data = band(self._y, (rshift(self.addr, 8) + 1))
    self.addr = bor(lshift(self.data, 8), band(self.addr, 0xff))
end

function CPU:_slo()
    self._p_c = rshift(self.data, 7)
    self.data = band(lshift(self.data, 1), 0xff)
    self._a = bor(self.data, self._a)
    self._p_nz = self._a
end

function CPU:_sre()
    self._p_c = band(self.data, 1)
    self.data = rshift(self.data, 1)
    self._a = bxor(self._a, self.data)
    self._p_nz = self._a
end

-- nops
function CPU:_nop()
end

-- interrupts
function CPU:_brk()
    local data = self._pc + 1
    self:push16(data)
    data = bor(self:flags_pack(), 0x10)
    self:push8(data)
    self._p_i = 0x04
    self.clk_irq = CPU.FOREVER_CLOCK
    self.clk = self.clk + CLK[7]
    local addr = self:fetch_irq_isr_vector() -- for inlining peek16
    self._pc = self:peek16(addr)
end

function CPU:_jam()
    self._pc = band((self._pc - 1), 0xffff)
    self.clk = self.clk + CLK[2]
    if not self.jammed then
        self.jammed = true
        -- interrupt reset
        self.clk_nmi = CPU.FOREVER_CLOCK
        self.clk_irq = CPU.FOREVER_CLOCK
        self.irq_flags = 0
    end
end

--------------------------------------------------------------------------------------------------------------------
-- default core
function CPU:printState(doFetch)
    --[
    local ppuclk = 0
    if self.ppu then
        ppuclk = self.ppu.hclk
    end
    print(
        string.format(
            "PC:%04X OP:%02X %02X %02X %s %04X %02X \t A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%3d %s",
            self._pc,
            self.opcode or 0,
            doFetch and self:fetch(self._pc + 1) or -1,
            doFetch and self:fetch(self._pc + 2) or -1,
            table.concat(CPU.DISPATCH[self.opcode] or {}, " "),
            self.data,
            self.addr,
            self._a,
            self._x,
            self._y,
            self:flags_pack(),
            self._sp,
            self.clk / 4 % 341,
            tostring(ppuclk),
            self.clk
        )
    )
    --]]
    --[[
    print(
        string.format(
            "PC:%04X OP:%02X %02X %02X %s %04X %02X \t A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%3d %d %d %d",
            self._pc,
            self.opcode or 0,
            doFetch and self:fetch(self._pc + 1) or -1,
            doFetch and self:fetch(self._pc + 2) or -1,
            table.concat(CPU.DISPATCH[self.opcode] or {}, " "),
            self.data,
            self.addr,
            self._a,
            self._x,
            self._y,
            self:flags_pack(),
            self._sp,
            self.clk / 4 % 341,
            self.clk,
            self.clk_target,
            self.clk_frame
        )
    )
    --]]
end
function CPU:r_op(instr, mode)
    self[mode](self, true, false)
    self[instr](self)
end

function CPU:w_op(instr, mode, store)
    self[mode](self, false, true)
    self[instr](self)
    self[store](self)
end

function CPU:rw_op(instr, mode, store)
    self[mode](self, true, true)
    self[instr](self)
    self[store](self)
end

function CPU:a_op(instr)
    self.clk = self.clk + CLK[2]
    self.data = self._a
    self[instr](self)
    self._a = self.data
end

function CPU:no_op(_instr, ops, ticks)
    self._pc = self._pc + ops
    self.clk = self.clk + ticks * CPU.RP2A03_CC
end

function CPU:do_clock()
    local ticks = self.apu:do_clock()
    local clock = math.min(ticks, self.clk_frame)
    --print "do clock"
    --print(clock)
    --printf("%04X", clock)

    --[[
    printf("%04X", clock)
    printf("%04X", self.clk_nmi)
    printf("%04X", self.clk_irq)
    print(self.clk < self.clk_nmi)
    print(self.clk < self.clk_irq)
    ]]
    --print "cpu do clock"
    if self.clk < self.clk_nmi then
        clock = math.min(clock, self.clk_nmi)
        if self.clk < self.clk_irq then
            clock = math.min(clock, self.clk_irq)
        else
            --print "cpu do isr irq"
            self.clk_irq = CPU.FOREVER_CLOCK
            self:do_isr(CPU.IRQ_VECTOR)
        end
    else
        --print "cpu do isr nmi"
        self.clk_nmi = CPU.FOREVER_CLOCK
        self.clk_irq = CPU.FOREVER_CLOCK
        self:do_isr(CPU.NMI_VECTOR)
    end
    --print(self.clk)
    self.clk_target = clock
end
local asd = 0
function CPU:run_once()
    self.opcode = self:fetch(self._pc)
    if self.conf.loglevel >= 3 then
        self:printState(true)
    --[[
            self.conf.debug(string.format("PC:%04X A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%3d : OPCODE:%02X (%d, %d)" ,
              self._pc, self._a, self._x, self._y, self:flags_pack(), self._sp, self.clk / 4 % 341, self.opcode, self.cl
            ))]]
    end

    self._pc = self._pc + 1

    --[[
    CPU.DISPATCHER[self.opcode](self)
    ]]
    local operationData = CPU.DISPATCH[self.opcode]
    local f = operationData[1]
    self[f](self, unpack(operationData, 2))
    --print(self.clk)
    if self.ppu_sync then
        self.ppu:sync(self.clk)
    end
    --print(self.clk)
    asd = asd + 1
    if asd > 350000000 then
        if asdasdsssasd then
            asdasdsssasd:flush()
            asdasdsssasd:close()
        end
        error "asd"
    end
end
function CPU:run()
    --print "run"
    --print(0xfffc)
    --print(self._pc)
    self:do_clock()
    repeat
        repeat
            self:run_once() --[[
            printf("STEP1")
            printf("%04X", self.clk)
            printf("%04X", self.clk_target)
            printf("%04X", self.clk_frame)
            ]]
        until not (self.clk < self.clk_target)
        --[[
        printf("STEP2")
        printf("%04X", self.clk_target)
        ]]
        self:do_clock()
    until not (self.clk < self.clk_frame)
    --[[
    printf("STEP3")
    printf("%04X", self.clk_frame)
    ]]
end

CPU.ADDRESSING_MODES = {
    ctl = {"imm", "zpg", "imm", "abs", UNDEFINED, "zpg_x", UNDEFINED, "abs_x"},
    rmw = {"imm", "zpg", "imm", "abs", UNDEFINED, "zpg_y", UNDEFINED, "abs_y"},
    alu = {"ind_x", "zpg", "imm", "abs", "ind_y", "zpg_x", "abs_y", "abs_x"},
    uno = {"ind_x", "zpg", "imm", "abs", "ind_y", "zpg_y", "abs_y", "abs_y"}
}
CPU.DISPATCH = {}

-- add an op/instruction
-- opcodes is a table or a single intruction code
-- args is a table of strings or a string of the form {kind,funcName,addrmode} or funcName
local function op(opcodes, args)
    for _, opcode in ipairs(opcodes) do
        local send_args
        if type(args) == "table" then
            if (args[1] == "r_op" or args[1] == "w_op" or args[1] == "rw_op") then
                local kind, ope, addrmode = args[1], args[2], args[3]
                addrmode = CPU.ADDRESSING_MODES[addrmode][band(rshift(opcode, 2), 7) + 1]
                send_args = {kind, ope, addrmode}
                if kind ~= "r_op" then
                    send_args[#send_args + 1] = (addrmode:sub(1, 3) == "zpg" and "store_zpg" or "store_mem")
                end
            else
                send_args = args
            end
        else
            send_args = {args}
        end
        CPU.DISPATCH[opcode] = send_args
    end
end

-- load instructions
op({0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1}, {"r_op", "_lda", "alu"})
op({0xa2, 0xa6, 0xb6, 0xae, 0xbe}, {"r_op", "_ldx", "rmw"})
op({0xa0, 0xa4, 0xb4, 0xac, 0xbc}, {"r_op", "_ldy", "ctl"})

-- store instructions
op({0x85, 0x95, 0x8d, 0x9d, 0x99, 0x81, 0x91}, {"w_op", "_sta", "alu"})
op({0x86, 0x96, 0x8e}, {"w_op", "_stx", "rmw"})
op({0x84, 0x94, 0x8c}, {"w_op", "_sty", "ctl"})

-- transfer instructions
op({0xaa}, "_tax")
op({0xa8}, "_tay")
op({0x8a}, "_txa")
op({0x98}, "_tya")
-- flow control instructions
op({0x4c}, "_jmp_a")
op({0x6c}, "_jmp_i")
op({0x20}, "_jsr")
op({0x60}, "_rts")
op({0x40}, "_rti")
op({0xd0}, "_bne")
op({0xf0}, "_beq")
op({0x30}, "_bmi")
op({0x10}, "_bpl")
op({0xb0}, "_bcs")
op({0x90}, "_bcc")
op({0x70}, "_bvs")
op({0x50}, "_bvc")

-- math operations
op({0x69, 0x65, 0x75, 0x6d, 0x7d, 0x79, 0x61, 0x71}, {"r_op", "_adc", "alu"})
op({0xe9, 0xeb, 0xe5, 0xf5, 0xed, 0xfd, 0xf9, 0xe1, 0xf1}, {"r_op", "_sbc", "alu"})

-- logical operations
op({0x29, 0x25, 0x35, 0x2d, 0x3d, 0x39, 0x21, 0x31}, {"r_op", "_and", "alu"})
op({0x09, 0x05, 0x15, 0x0d, 0x1d, 0x19, 0x01, 0x11}, {"r_op", "_ora", "alu"})
op({0x49, 0x45, 0x55, 0x4d, 0x5d, 0x59, 0x41, 0x51}, {"r_op", "_eor", "alu"})
op({0x24, 0x2c}, {"r_op", "_bit", "alu"})
op({0xc9, 0xc5, 0xd5, 0xcd, 0xdd, 0xd9, 0xc1, 0xd1}, {"r_op", "_cmp", "alu"})
op({0xe0, 0xe4, 0xec}, {"r_op", "_cpx", "rmw"})
op({0xc0, 0xc4, 0xcc}, {"r_op", "_cpy", "rmw"})

-- shift operations
op({0x0a}, {"a_op", "_asl"})
op({0x06, 0x16, 0x0e, 0x1e}, {"rw_op", "_asl", "alu"})
op({0x4a}, {"a_op", "_lsr"})
op({0x46, 0x56, 0x4e, 0x5e}, {"rw_op", "_lsr", "alu"})
op({0x2a}, {"a_op", "_rol"})
op({0x26, 0x36, 0x2e, 0x3e}, {"rw_op", "_rol", "alu"})
op({0x6a}, {"a_op", "_ror"})
op({0x66, 0x76, 0x6e, 0x7e}, {"rw_op", "_ror", "alu"})

-- increment and decrement operations
op({0xc6, 0xd6, 0xce, 0xde}, {"rw_op", "_dec", "alu"})
op({0xe6, 0xf6, 0xee, 0xfe}, {"rw_op", "_inc", "alu"})
op({0xca}, "_dex")
op({0x88}, "_dey")
op({0xe8}, "_inx")
op({0xc8}, "_iny")

-- flags instructions
op({0x18}, "_clc")
op({0x38}, "_sec")
op({0xd8}, "_cld")
op({0xf8}, "_sed")
op({0x58}, "_cli")
op({0x78}, "_sei")
op({0xb8}, "_clv")

-- stack operations
op({0x48}, "_pha")
op({0x08}, "_php")
op({0x68}, "_pla")
op({0x28}, "_plp")
op({0xba}, "_tsx")
op({0x9a}, "_txs")

-- undocumented instructions, rarely used
op({0x0b, 0x2b}, {"r_op", "_anc", "uno"})
op({0x8b}, {"r_op", "_ane", "uno"})
op({0x6b}, {"r_op", "_arr", "uno"})
op({0x4b}, {"r_op", "_asr", "uno"})
op({0xc7, 0xd7, 0xc3, 0xd3, 0xcf, 0xdf, 0xdb}, {"rw_op", "_dcp", "alu"})
op({0xe7, 0xf7, 0xef, 0xff, 0xfb, 0xe3, 0xf3}, {"rw_op", "_isb", "alu"})
op({0xbb}, {"r_op", "_las", "uno"})
op({0xa7, 0xb7, 0xaf, 0xbf, 0xa3, 0xb3}, {"r_op", "_lax", "uno"})
op({0xab}, {"r_op", "_lxa", "uno"})
op({0x27, 0x37, 0x2f, 0x3f, 0x3b, 0x23, 0x33}, {"rw_op", "_rla", "alu"})
op({0x67, 0x77, 0x6f, 0x7f, 0x7b, 0x63, 0x73}, {"rw_op", "_rra", "alu"})
op({0x87, 0x97, 0x8f, 0x83}, {"w_op", "_sax", "uno"})
op({0xcb}, {"r_op", "_sbx", "uno"})
op({0x9f, 0x93}, {"w_op", "_sha", "uno"})
op({0x9b}, {"w_op", "_shs", "uno"})
op({0x9e}, {"w_op", "_shx", "rmw"})
op({0x9c}, {"w_op", "_shy", "ctl"})
op({0x07, 0x17, 0x0f, 0x1f, 0x1b, 0x03, 0x13}, {"rw_op", "_slo", "alu"})
op({0x47, 0x57, 0x4f, 0x5f, 0x5b, 0x43, 0x53}, {"rw_op", "_sre", "alu"})

-- nops
op({0x1a, 0x3a, 0x5a, 0x7a, 0xda, 0xea, 0xfa}, {"no_op", "_nop", 0, 2})
op({0x80, 0x82, 0x89, 0xc2, 0xe2}, {"no_op", "_nop", 1, 2})
op({0x04, 0x44, 0x64}, {"no_op", "_nop", 1, 3})
op({0x14, 0x34, 0x54, 0x74, 0xd4, 0xf4}, {"no_op", "_nop", 1, 4})
op({0x0c}, {"no_op", "_nop", 2, 4})
op({0x1c, 0x3c, 0x5c, 0x7c, 0xdc, 0xfc}, {"r_op", "_nop", "ctl"})
op({0x00}, "_brk")
op({0x02, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x92, 0xb2, 0xd2, 0xf2}, "_jam")
--[[
  Thiss aims to "cache" unpack (Maybe also "cache" self and op indexing?)
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
]]
return CPU
