require "nes"
Nes = nil
local width = 256
local height = 240
local pixSize = 2
function love.load(arg)
    local file = arg[1] or " "
    local loglvl = loadstring("return " .. (arg[2] or "0"))
    loglvl = loglvl and loglvl()
    local pc = loadstring("return " .. (arg[3] or ""))
    pc = pc and pc()
    imageData = love.image.newImageData(width * pixSize + 1, height * pixSize + 1)
    image = love.graphics.newImage(imageData)
    love.window.setTitle("LuaNEs")
    --Nes = NES:new({file="tests/hello.nes", loglevel=5})
    Nes = NES:new({file = file, loglevel = 0, pc = pc, loglevel = loglvl})
    --Nes:run()
    Nes:reset()
end

function love.draw()
    Nes:run_once()
    print "YEYEYEYEYE"
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
