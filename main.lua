if pcall(require, "jit.opt") then
    require("jit.opt").start(
        "maxmcode=8192",
        "maxtrace=2000"
    --
    )
end

if false then
    PROF_CAPTURE = true
    if pcall(require, "libs/jprof") then
        prof = require("libs/jprof")
    end
end
require "nes"
Nes = nil
local width = 256
local height = 240
local pixSize = 1
local lastSource
local sound = false
local IS_DEBUG = false

-- Just for IDE intellisense
if not love then
    love = {}
end

local loveErrorhandler = love.errorhandler or love.errhand
local vscode_debugger = os.getenv("LOCAL_LUA_DEBUGGER_VSCODE") == "1"
function love.errorhandler(msg)
    if vscode_debugger then
        error(msg, 2)
    else
        return loveErrorhandler(msg)
    end
end

function love.load(arg)
    if vscode_debugger then
        require("lldebugger").start()
    end
    --[[
    love.profiler = require("libs/profile")
    love.profiler.hookall("Lua")
    love.profiler.start()
    --]]
    local file = arg[1] or " "
    local loglvl = arg[2] and tonumber(arg[2]) or 0
    if arg[3] == "true" then
        IS_DEBUG = true
    end
    if vscode_debugger then
        IS_DEBUG = true
        loglvl = 3
    end
    local pc = nil
    if arg[4] and string.sub(arg[4], 1, 3) == "0x" then
        pc = tonumber(string.sub(arg[4], 3), 16)
    elseif arg[4] then
        pc = tonumber(arg[4])
    end
    imageData = love.image.newImageData(width * pixSize + 1, height * pixSize + 1)
    image = love.graphics.newImage(imageData)
    love.window.setTitle("LuaNEs")
    --Nes = NES:new({file="tests/hello.nes", loglevel=5})
    Nes =
        NES:new(
            {
                file = file,
                loglevel = loglvl,
                pc = pc,
                palette = UTILS.map(
                    PALETTE:defacto_palette(),
                    function(c)
                        return { c[1] / 256, c[2] / 256, c[3] / 256 }
                    end
                )
            }
        )
    --Nes:run()
    Nes:reset()
    love.window.setMode(width, height, { resizable = true, minwidth = width, minheight = height, vsync = false })
    local samplerate = 44100
    local bits = 16
    local channels = 1
    sound = love.sound.newSoundData(samplerate / 60 + 1, samplerate, bits, channels)
    QS = love.audio.newQueueableSource(samplerate, bits, channels)
end

local keyEvents = {}
local keyDownEventObjCache = {}
local keyUpEventObjCache = {}
local keyButtons = {
    ["w"] = Pad.UP,
    ["a"] = Pad.LEFT,
    ["s"] = Pad.DOWN,
    ["d"] = Pad.RIGHT,
    ["o"] = Pad.A,
    ["p"] = Pad.B,
    ["i"] = Pad.SELECT,
    ["return"] = Pad.START
}
function love.keypressed(key)
    if key == "t" then
        Nes.cpu.dbgPrint = true
    end
    if key == "m" then
        if not ProFi then
            ProFi = require("libs/ProFi")
            ProFi:checkMemory()
            ProFi:start()
        end
    end
    for k, v in pairs(keyButtons) do
        if k == key then
            if keyDownEventObjCache[v] == nil then
                keyDownEventObjCache[v] = { "keydown", v }
            end
            keyEvents[#keyEvents + 1] = keyDownEventObjCache[v]
        end
    end
end

function love.keyreleased(key)
    if key == "t" then
        Nes.cpu.dbgPrint = false
    end
    if key == "m" then
        if ProFi then
            ProFi:stop()
            ProFi:writeReport('MyProfilingReport.txt')
            Profi = nil
        end
    end
    for k, v in pairs(keyButtons) do
        if k == key then
            if keyUpEventObjCache[v] == nil then
                keyUpEventObjCache[v] = { "keyup", v }
            end
            keyEvents[#keyEvents + 1] = keyUpEventObjCache[v]
        end
    end
end

love.frame = 0
local time = 0
local timeTwo = 0
local update_freq = 59.94
local rate = 1 / update_freq
local max_updates_per_frame = update_freq * 1.2
local fpsHistory = UTILS.fill({}, 120, 120)
local fpsIdx = 1
local fps = 0
local tickRate = 0
local tickRatetmp = 0
local pixelCount = PPU.SCREEN_HEIGHT * PPU.SCREEN_WIDTH
local function update()
    drawn = true
    tickRatetmp = tickRatetmp + 1
    for i, v in ipairs(keyEvents) do
        Nes.pads[v[1]](Nes.pads, 1, v[2])
    end
    table.clear(keyEvents)
    Nes:run_once()
    local samples = Nes.cpu.apu.output
    for i = 1, #samples do
        sound:setSample(i, samples[i])
    end
    QS:queue(sound)
    QS:play()
end
local function drawScreen()
    local sx = love.graphics.getWidth() / image:getWidth()
    local sy = love.graphics.getHeight() / image:getHeight()
    love.graphics.draw(image, 0, 0, 0, sx, sy)
    love.graphics.print(" Nes Tick Rate: " .. tostring(tickRate), 10, 10)
    love.graphics.print(" FPS: " .. tostring(fps), 10, 30)
end
local function drawPalette()
    local palette = Nes.cpu.ppu.output_color
    local w, h = 10, 10
    local x, y = 0, 50
    local row, column = 4, 8
    for i = 1, #palette do
        local px = palette[i]
        if px then
            local r = px[1]
            local g = px[2]
            local b = px[3]
            love.graphics.setColor(r, g, b, 1)
            love.graphics.rectangle("fill", x + ((i - 1) % row) * w, y + math.floor((i - 1) / 4) * h, w, h)
        end
    end
    love.graphics.setColor(1, 1, 1, 1)
end
local function drawAPUState()
    local apu = Nes.cpu.apu
    love.graphics.print(" Pulse 1", 10, 140)
    local pulse_0 = apu.pulse_0
    love.graphics.print(
        string.format(
            "F:%d D:%d V:%d  S:%d C:%d",
            pulse_0.freq / 1000,
            pulse_0.duty,
            pulse_0.envelope.output / APU.CHANNEL_OUTPUT_MUL,
            pulse_0.step,
            pulse_0.length_counter.count
        ),
        10,
        160
    )
    love.graphics.print(" Pulse 2", 10, 180)
    local pulse_1 = apu.pulse_1
    love.graphics.print(
        string.format(
            "F:%d D:%d V:%d  S:%d C:%d",
            pulse_1.freq / 1000,
            pulse_1.duty,
            pulse_1.envelope.output / APU.CHANNEL_OUTPUT_MUL,
            pulse_1.step,
            pulse_1.length_counter.count
        ),
        10,
        200
    )
    --love.graphics.print(string.format("MMC5: %04X", Nes.rom.ppu_nametable_mappings_reg), 10, 220)
    love.graphics.print(string.format("garbage count(LUA): %d", collectgarbage("count")), 10, 220)
    --collectgarbage("collect")
end
local function draw()
    drawScreen()
    if IS_DEBUG then
        drawPalette()
        drawAPUState()
    end
end
function love.draw()
    --prof.push("frame")
    --[
    time = time + love.timer.getDelta()
    timeTwo = timeTwo + love.timer.getDelta()
    --[[
    --]]
    local update_count = 0
    while time > rate and update_count < max_updates_per_frame do
        time = time - rate
        update()
        update_count = update_count + 1
    end
    --update()
    if timeTwo > 1 then
        timeTwo = 0
        tickRate = tickRatetmp
        tickRatetmp = 0
    end
    fps = 1 / love.timer.getDelta()
    fpsHistory[fpsIdx] = fps
    fpsIdx = (fpsIdx % #fpsHistory) + 1
    fps = 0
    for i = 1, #fpsHistory do
        fps = fps + fpsHistory[i]
    end
    fps = fps / #fpsHistory
    --]]
    --[[
    timeTwo = timeTwo + love.timer.getDelta()
    if timeTwo > 1 then
        timeTwo = 0
        tickRate = tickRatetmp
        tickRatetmp = 0
    end
    update()
    --]]
    --[
    local pxs = Nes.cpu.ppu.output_pixels
    for i = 1, pixelCount do
        local x = (i - 1) % width
        local y = math.floor((i - 1) / width) % height
        local px = pxs[i]
        --[[
        local r = rshift(band(px, 0x00ff0000), 16)
        local g = rshift(band(px, 0x0000ff00), 8)
        local b = band(px, 0x000000ff)
        --]]
        --[[
        local r = px[1]
        local g = px[2]
        local b = px[3]
        for j = 0, pixSize - 1 do
            for k = 0, pixSize - 1 do
                local xx = 1 + pixSize * (x) + j
                local yy = 1 + pixSize * (y) + k
                imageData:setPixel(xx, yy, r, g, b, 1)
            end
        end
        --]]
        --[
        imageData:setPixel(x + 1, y + 1, px[1], px[2], px[3], 1)
        --]]
    end
    image:replacePixels(imageData)

    draw()
    --prof.pop("frame")
end

function love.quit()
    if prof then
        prof.write("prof.mpack")
    end
    return false
end
