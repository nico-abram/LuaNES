require "serpent"
require "utils"
require "cpu"
require "ppu"
require "rom"
require "palette"

UTILS:import()

NES = {}
local NES = NES
NES._mt = {__index = NES}

function NES:reset()
    self.audio, self.video, self.input = {spec = {}}, {palette = {}}, {}

    self.cpu:reset()
    self.cpu.apu:reset(self.audio.spec)
    self.cpu.ppu:reset(self.video.palette)
    self.rom:reset()
    self.pads:reset()
    local cpu = self.cpu
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
function NES:new(file)
    local conf = {romfile = file, loglevel = 5}
    local nes = {}
    setmetatable(nes, NES._mt)
    nes.cpu = CPU.new()
    nes.cpu.apu = {
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
    nes.cpu.ppu = PPU:new(conf, nes.cpu, PALETTE:defacto_palette())
    nes.pads = {
        reset = function()
        end
    }
    nes.rom = ROM.load(conf, nes.cpu, nes.cpu.ppu)

    nes.frame = 0
    nes.frame_target = nil
    return nes
end
