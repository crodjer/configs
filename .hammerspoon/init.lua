-- Modifier to be used across Hammerspoon bindings.
local hsModifier = { "ctrl", "alt" }
local logger = hs.logger.new('init.lua', 'info')

-- List of apps and their screeen / binding, configuration
local appList = {
    Alacritty = { binding = "t" },
    Peek = { binding = "p" },
    ["Android Studio"] = { binding = "a" },
    qemu = { binding = "q" },
    ["IntelliJ IDEA"] = { binding = "e" },
    ["Brave Browser"] = { binding = "b" },
    ["zoom.us"] = { binding = "o", legacyActivate = true },
    Slack = { binding = "s" },
    Postman = { binding = "r" },
    ["Amazon Music"]= { binding = "," },
    Firefox = { binding = "f" },
    Signal = { binding = "g" },
    Notes = { binding = "n" },
    ["Google Chrome"]= { binding = "c" },
    Messages = {
        -- This is Google Messages
        binding = "0",
        autoLaunch = true,
        bundleID ="com.brave.Browser.app.hpfldicfbfomlpcikngkocigghgafkph"
    },
    ["Google Duo"] = {
        binding = "9",
        autoLaunch = true,
        bundleID ="com.brave.Browser.app.imgohncinckhbblnlmaedahepnnpmdma"
    },
}

function renderTable(table)
    rendered = ""
    for k,v in pairs(table) do
        rendered = rendered .. "\n" .. k .. " => " .. tostring(v)
    end
    print(rendered)
end

-- Caffeine: A button in the menu bar.
hs.loadSpoon("Caffeine")
spoon.Caffeine:start()

-- WindowScreenLeftAndRight: Shorcut to move windows through screens.
hs.loadSpoon("WindowScreenLeftAndRight")
spoon.WindowScreenLeftAndRight:bindHotkeys({
   screen_left = { hsModifier, "[" },
   screen_right= { hsModifier, "]" },
})

-- ReloadConfiguration: Auto reload configuration for Hammerspoon
hs.loadSpoon("ReloadConfiguration")
spoon.ReloadConfiguration.watch_paths = { hs.configdir, "~/Documents/configs/.hammerspoon" }
spoon.ReloadConfiguration:start()

-- Seal: The awesome seal plugin, with pasteboard (pb) support.
hs.loadSpoon("Seal")
spoon.Seal:loadPlugins({ "apps", "pasteboard" })
spoon.Seal.plugins.pasteboard.historySize = 10
spoon.Seal.plugins.pasteboard.saveHistory = false

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
hs.hotkey.bind(hsModifier, 'tab', nil, hs.window.switcher.nextWindow)
hs.hotkey.bind({ "shift", table.unpack(hsModifier)}, 'tab', nil, hs.window.switcher.previousWindow)

hs.alert.defaultStyle.radius = 10
hs.alert.defaultStyle.atScreenEdge = 2
hs.alert.defaultStyle.fillColor = { white = 0, alpha = 0.5 }
hs.alert.defaultStyle.textSize = 15
hs.alert.defaultStyle.fadeInDuration = 0.1
hs.alert.defaultStyle.fadeOutDuration = 0.1

function activateApp(application, config)
    if config.legacyActivate then
        hs.application.open(application:name())
    else
        application:activate()
    end
end

function launchApp(app, config)
    if config.bundleID then
        hs.application.launchOrFocusByBundleID(config.bundleID)
    else
        hs.application.launchOrFocus(app)
    end
end

appNotRunningAlertId = nil

-- Attach app bindings
for app, config in pairs(appList) do
    -- Bind the app 
    if config.binding ~= nil then
       hs.hotkey.bind(hsModifier, config.binding, function()
          application = hs.application.get(config.bundleID or app)

          if application then
              activateApp(application, config)
          elseif config.autoLaunch then
              launchApp(app, config)
          else
             hs.alert.closeSpecific(appNotRunningAlertId, 0)
             appNotRunningAlertId = hs.alert(app .. " not running!", nil, nil, 1)
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
    f.w = screen.w * 0.55
    f.h = screen.h
    win:setFrame(f)
end)

hs.hotkey.bind(hsModifier, ";", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    f.x = screen.x  + screen.w * 0.55
    f.y = screen.y
    f.w = screen.w * 0.45
    f.h = screen.h
    win:setFrame(f)
end)


hs.hotkey.bind(hsModifier, "k", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    f.y = screen.y + screen.h * 0.5
    f.h = screen.h * 0.5
    win:setFrame(f)
end)

hs.hotkey.bind(hsModifier, "l", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    f.y = screen.y
    f.h = screen.h * 0.5
    win:setFrame(f)
end)
