local vscode_debugger = os.getenv("LOCAL_LUA_DEBUGGER_VSCODE") == "1"
function love.conf(t)
    t.identity = "LuaNES"
    if not vscode_debugger then
        t.console = true
    end
end
