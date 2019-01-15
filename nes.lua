require "libs/serpent"
require "utils"
NES = {}
local NES = NES
NES._mt = {__index = NES}
require "cpu"
require "ppu"
require "apu"
require "rom"
require "palette"

UTILS:import()

function NES:reset()
    self.audio, self.video, self.input = {spec = {}}, {palette = {}}, {}

    local cpu = self.cpu
    cpu:reset()
    cpu.apu:reset(self.audio.spec)
    cpu.ppu:reset()
    self.rom:reset()
    self.pads:reset()
    cpu:boot()
    self.rom:load_battery()
end
function NES:run_once()
    self.cpu.ppu:setup_frame()
    self.cpu:run()
    self.cpu.ppu:vsync()
    self.cpu.apu:vsync()
    self.cpu:vsync()
    self.rom:vsync()

    self.frame = self.frame + 1
end
function NES:run()
    self:reset()
    while true do
        self:run_once()
    end
end
function NES:new(opts)
    opts = opts or {}
    local conf = {romfile = opts.file, pc = opts.pc or nil, loglevel = opts.loglevel or 0}
    local nes = {}
    local palette = PALETTE:defacto_palette()
    setmetatable(nes, NES._mt)
    nes.cpu = CPU:new(conf)
    nes.cpu.apu = APU:new(conf, nes.cpu)
    --[[
        clock_dma = function(clk)
        end,
        reset = function()
        end,
        vsync = function()
        end,
        do_clock = function()
            return CPU.CLK[1]
        end
    }
    ]]
    --[[
    nes.cpu.ppu = {
        reset = function()
        end,
        vsync = function()
        end,
        setup_frame = function()
        end,
        sync = function(clk)
        end
    }
    --]]
    nes.cpu.ppu = PPU:new(conf, nes.cpu, palette)
    nes.pads = {
        reset = function()
        end
    }
    nes.rom = ROM.load(conf, nes.cpu, nes.cpu.ppu)

    nes.frame = 0
    nes.frame_target = nil
    return nes
end
