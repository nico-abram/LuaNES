require "nes"
Nes = nil
local width = 256
local height = 240
local pixSize = 2
function love.load()
    imageData = love.image.newImageData(width * pixSize + 1, height * pixSize + 1)
    image = love.graphics.newImage(imageData)
    love.graphics.setCanvas(canvas)
    love.graphics.clear()
    love.graphics.setBlendMode("alpha")
    love.graphics.setColor(1, 0, 0, 0.5)
    love.graphics.rectangle("fill", 0, 0, 100, 100)
    love.graphics.setCanvas()
    love.window.setTitle("Title Goes Here...")
    Nes = NES:new("tests/nestest.nes")
    Nes:run()
    Nes:reset()
end

function love.draw()
    Nes.cpu.ppu:setup_frame()
    Nes.cpu:run_once()
    Nes.cpu.ppu:vsync()
    Nes.cpu.apu:vsync()
    Nes.cpu:vsync()
    Nes.rom:vsync()

    Nes.frame = Nes.frame + 1
    -- very important!: reset color before drawing to canvas to have colors properly displayed
    -- see discussion here: https://love2d.org/forums/viewtopic.php?f=4&p=211418#p211418
    love.graphics.setColor(1, 1, 1, 1)
    local pxs = Nes.cpu.ppu.output_pixels
    local max = 0
    for i = 1, #pxs do
        local x = (i - 1) % width
        local y = math.floor((i - 1) / width) % height
        for j = 0, pixSize - 1 do
            for k = 0, pixSize - 1 do
                local xx = 1 + pixSize * (x) + j
                local yy = 1 + pixSize * (y) + k
                max = math.max(max, xx)
                imageData:setPixel(xx, yy, pxs[i][1], pxs[i][2], pxs[i][3], 1)
            end
        end
    end
    --image = love.graphics.newImage(imageData)
    image:replacePixels(imageData)
    love.graphics.draw(image, 0, 0)
end
