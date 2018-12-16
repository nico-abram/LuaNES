require "nes"
function love.load()
    love.window.setTitle("Title Goes Here...")
    local nes = NES:new("nestest.nes")
    nes:run()
end
