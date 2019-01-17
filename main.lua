require "nes"
Nes = nil
local width = 256
local height = 240
local pixSize = 1
function love.load(arg)
    --[[
    love.profiler = require("libs/profile")
    love.profiler.hookall("Lua")
    love.profiler.start()
    --]]
    local file = arg[1] or " "
    local loglvl = loadstring("return " .. (arg[2] or "0"))
    loglvl = loglvl and loglvl()
    local pc = loadstring("return " .. (arg[3] or ""))
    pc = pc and pc()
    imageData = love.image.newImageData(width * pixSize + 1, height * pixSize + 1)
    image = love.graphics.newImage(imageData)
    love.window.setTitle("LuaNEs")
    --Nes = NES:new({file="tests/hello.nes", loglevel=5})
    Nes = NES:new({
        file = file, 
        loglevel = loglvl, 
        pc = pc, 
        palette = UTILS.map(
            PALETTE:defacto_palette(), 
            function(c) 
                return {c[1]/256,c[2]/256,c[3]/256}
            end
            )
        }
    )
    --Nes:run()
    Nes:reset()
end
local keyEvents = {}
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
function love.keypressed( key )
    for k,v in pairs(keyButtons) do
        if k == key then
      keyEvents[#keyEvents+1] = {"keydown", v}
        end
    end
end
 
function love.keyreleased( key )
    for k,v in pairs(keyButtons) do
        if k == key then
      keyEvents[#keyEvents+1] = {"keyup", v}
        end
    end
end

love.frame = 0
function love.draw()
    for i,v in ipairs(keyEvents) do
        Nes.pads[v[1]](Nes.pads,1, v[2])
    end
    keyEvents = {}
    Nes:run_once()
    --[
    local pxs = Nes.cpu.ppu.output_pixels
    for i = 1, #pxs do
        local x = (i - 1) % width
        local y = math.floor((i - 1) / width) % height
        local px = pxs[i]
        --[[
        local r = bit.rshift(band(px, 0x00ff0000), 16)
        local g = bit.rshift(band(px, 0x0000ff00), 8)
        local b = band(px, 0x000000ff)
        --]]
        --[[
        local r = px[1]
        local g = px[2]
        local b = px[3]
        --]]
        local xx = 1 + pixSize * (x)
        local yy = 1 + pixSize * (y)
        imageData:setPixel(xx, yy, px[1], px[2], px[3], 1)
        --[[
        for j = 0, pixSize - 1 do
            for k = 0, pixSize - 1 do
                local xx = 1 + pixSize * (x) + j
                local yy = 1 + pixSize * (y) + k
                imageData:setPixel(xx, yy, r, g, b, 1)
            end
        end
        --]]
    end
    -- draw palette
    --[[
    local palette = Nes.cpu.ppu.output_color
    local w,h = 10,10
    local x,y = 700,500
    local row,column = 4, 8
    for i=1,#palette do
        local px = palette[i]
        if px then 
        local r = px[1] / 256
        local g = px[2] / 256
        local b = px[3] / 256
        love.graphics.setColor( r, g, b, 1 )
        love.graphics.rectangle("fill", x+((i-1)%row)*w, y+math.floor((i-1)/4)*h, w, h )
        end
    end
        love.graphics.setColor( 1, 1, 1, 1 )
    --]]
    --image = love.graphics.newImage(imageData)
    image:replacePixels(imageData)
    love.graphics.draw(image, 50, 50)
    --]]
    --[[
    love.frame = love.frame + 1
    if love.frame % 5 == 0 then
        love.report = love.profiler.report("time", 20)
        print(love.report)
        a = 0 / 0
        love.profiler.reset()
    end
    --]]
    love.graphics.print("Current FPS: " .. tostring(love.timer.getFPS()), 10, 10)
end
