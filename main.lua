require "nes"
function love.load()
    love.window.setTitle("Title Goes Here...")
    local nes = NES:new("Lawn_Mower.nes")
    nes:run()
end
