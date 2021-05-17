-- Modifier to be used across Hammerspoon bindings.
local hsModifier = { "ctrl", "alt" }
local logger = hs.logger.new('init.lua', 'info')

-- List of apps and their screeen / binding, configuration
local appList = {
    Alacritty = { binding = "t" },
    Alsi = { binding = "1" },
    Alpi = { binding = "2" },
    ["Android Studio"] = { binding = "a" },
    qemu = { binding = "q" },
    ["IntelliJ IDEA"] = { binding = "e" },
    ["Brave Browser"] = { binding = "b" },
    ["zoom.us"] = { binding = "o" },
    Slack = { binding = "s" },
    Postman = { binding = "r" },
    ["Amazon Music"]= { binding = "," },
    Firefox = { binding = "f" },
    Signal = { binding = "g" },
    Notes = { binding = "n" },
    Messages = { binding = "0" },
}

-- -- Calendar: A nice calendar on the desktop.
hs.loadSpoon("Calendar")
-- -- CircleClock: A nice clock on the desktop.
hs.loadSpoon("CircleClock")

-- Caffeine: A button in the menu bar.
hs.loadSpoon("Caffeine")
spoon.Caffeine:start()

-- ReloadConfiguration: Auto reload configuration for Hammerspoon
hs.loadSpoon("ReloadConfiguration")
spoon.ReloadConfiguration.watch_paths = { hs.configdir, "~/Documents/configs/.hammerspoon" }
spoon.ReloadConfiguration:start()

-- WindowScreenLeftAndRight: Shorcut to move windows through screens.
hs.loadSpoon("WindowScreenLeftAndRight")
spoon.WindowScreenLeftAndRight:bindHotkeys({
   screen_left = { hsModifier, "[" },
   screen_right= { hsModifier, "]" },
})

-- Seal: The awesome seal plugin, with pasteboard (pb) support.
hs.loadSpoon("Seal")
spoon.Seal:loadPlugins({ "apps", "pasteboard" })
spoon.Seal.plugins.pasteboard.historySize = 10
spoon.Seal.plugins.pasteboard.saveHistory = false

-- spoon.Seal.plugins.useractions.actions = {
--    ["Heimdall Jira"] = {
--       url = "https://ifountain.atlassian.net/browse/HEIMDALL-${query}",
--       keyword = "hj"
--    },
--    ["OG Support"] = {
--       url = "https://ifountain.atlassian.net/browse/OGS-${query}",
--       keyword = "ogs"
--    }
-- }

-- spoon.Seal.plugins.urlformats:providersTable({
--    hj = { name = "Heimdall Jira", url = "https://ifountain.atlassian.net/browse/HEIMDALL-%s" }
-- })
spoon.Seal:bindHotkeys({
    toggle = { {"cmd"}, "space" }
})
spoon.Seal:start()

-- Switcher
hs.window.switcher.ui.titleBackgroundColor = {0, 0, 0, 0}
hs.window.switcher.ui.fontName = 'Verdana'
hs.window.switcher.ui.textSize = 12
hs.window.switcher.ui.showThumbnails = false
hs.window.switcher.ui.showSelectedThumbnail = false
-- hs.hotkey.bind(hsModifier, 'tab', nil, hs.window.switcher.nextWindow)
-- hs.hotkey.bind({ "shift", table.unpack(hsModifier)}, 'tab', nil, hs.window.switcher.previousWindow)

hs.alert.defaultStyle.radius = 10
hs.alert.defaultStyle.atScreenEdge = 2
hs.alert.defaultStyle.fillColor = { white = 0, alpha = 0.5 }
hs.alert.defaultStyle.textSize = 15
hs.alert.defaultStyle.fadeInDuration = 0.1
hs.alert.defaultStyle.fadeOutDuration = 0.1

-- Hide all applications
-- hs.hotkey.bind(hsModifier, "h", function()
--    function hideApps()
--       for _, app in pairs(hs.application.runningApplications()) do
--          app:hide()
--       end
--    end
-- 
--    hideApps()
--    -- IDEA/Brave misbehave, so try hiding them once again.
--    hs.timer.doAfter(0.1, hideApps)
-- end)

alertId = nil

-- Spaces
-- watcher = hs.spaces.watcher.new(function (space)
--     print("Space", space)
-- end)
-- watcher:start()

-- Attach app bindings
for app, config in pairs(appList) do
    -- Bind the app 
    if config.binding ~= nil then
       hs.hotkey.bind(hsModifier, config.binding, function()
          application = hs.application.find(app)
          if config.autoHide then
             -- Do nothing.
          elseif application then
             application:setFrontmost()
          else
             hs.alert.closeSpecific(alertID, 0)
             alertId = hs.alert(app .. " not running!", nil, nil, 1)
          end
       end)
    end
end

hs.window.animationDuration = 0

hs.hotkey.bind(hsModifier, "m", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    f.x = screen.x
    f.y = screen.y
    f.w = screen.w
    f.h = screen.h
    win:setFrame(f)
end)

hs.hotkey.bind(hsModifier, "j", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    f.x = screen.x
    f.y = screen.y
    f.w = screen.w * 0.5
    f.h = screen.h
    win:setFrame(f)
end)

hs.hotkey.bind(hsModifier, ";", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    f.x = screen.x  + screen.w * 0.5
    f.y = screen.y
    f.w = screen.w * 0.5
    f.h = screen.h
    win:setFrame(f)
end)


hs.hotkey.bind(hsModifier, "k", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    f.x = screen.x
    f.y = screen.y + screen.h * 0.5
    f.w = screen.w
    f.h = screen.h * 0.5
    win:setFrame(f)
end)

hs.hotkey.bind(hsModifier, "l", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    f.x = screen.x
    f.y = screen.y
    f.w = screen.w
    f.h = screen.h * 0.5
    win:setFrame(f)
end)
