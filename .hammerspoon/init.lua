-- Modifier to be used across Hammerspoon bindings.
local hsModifier = { "ctrl", "alt" }
local hsShift = { "ctrl", "alt", "shift" }
local logger = hs.logger.new('init.lua', 'info')

hs.window.animationDuration = 0

-- List of apps and their screeen / binding, configuration
local appList = {
    Alacritty = { binding = "t" },
    Peek = { binding = "i" },
    ["Android Studio"] = { binding = "a" },
    ["IntelliJ IDEA"] = { binding = "e" },
    ["Brave Browser"] = { binding = "b" },
    ["zoom.us"] = { binding = "o" },
    Slack = { binding = "s" },
    Postman = { binding = "r" },
    ["Amazon Music"]= { binding = "," },
    Firefox = { binding = "f" },
    Signal = { binding = "g" },
    Notes = { binding = "n" },
    Bitwarden = { binding = "p", legacyActivate = true },
    ["Google Chrome"]= { binding = "c" },

    Messages = {
        -- This is Google Messages
        binding = "0",
        autoLaunch = true,
        bundleID ="com.brave.Browser.app.hpfldicfbfomlpcikngkocigghgafkph"
    },
    YouTube = {
        binding = "y",
        autoLaunch = true,
        bundleID ="com.brave.Browser.app.agimnkijcaahngcdmfeangaknmldooml"
    },
    ["Google Duo"] = {
        binding = "7",
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

-- Cherry: Pomodoro Timer
hs.loadSpoon("Cherry")
spoon.Cherry.duration = 25
spoon.Cherry.alertTextSize = 50
spoon.Cherry.notification = hs.notify.new({ title = "Timer's up. Have some rest!", withdrawAfter = 10})
spoon.Cherry.sound = hs.sound.getByFile("/System/Library/Sounds/Submarine.aiff")
spoon.Cherry:bindHotkeys({ start = { hsModifier, "=" }})
spoon.Cherry:reset()
-- spoon.Cherry:start()

-- Start cherry on unlock.
cherryWatcher = hs.caffeinate.watcher.new(function(event) 
    if event == hs.caffeinate.watcher.screensDidUnlock then
        spoon.Cherry:reset()
        spoon.Cherry:start()
    end
end)
-- cherryWatcher:start()

-- WindowScreenLeftAndRight: Shorcut to move windows through screens.
hs.loadSpoon("WindowScreenLeftAndRight")
spoon.WindowScreenLeftAndRight:bindHotkeys({
   screen_left = { hsModifier, "[" },
   screen_right= { hsModifier, "]" },
})

-- ReloadConfiguration: Auto reload configuration for Hammerspoon
hs.loadSpoon("ReloadConfiguration")
spoon.ReloadConfiguration.watch_paths = { 
    hs.configdir .. "/init.lua",
    hs.configdir .. "/Spoons"
}
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
hs.window.animationDuration = 0
switcher = hs.window.switcher
switcher.ui.titleBackgroundColor = {0, 0, 0, 0}
switcher.ui.fontName = 'Verdana'
switcher.ui.textSize = 12
switcher.ui.showThumbnails = false
switcher.ui.showSelectedThumbnail = false
hs.hotkey.bind(hsModifier, 'h', nil, switcher.previousWindow)
hs.hotkey.bind(hsModifier, '\'', nil, switcher.nextWindow)
hs.hotkey.bind(hsShift, 'tab', nil, switcher.previousWindow)
hs.hotkey.bind(hsModifier, 'tab', nil, switcher.nextWindow)

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


-- function winLayout(layout)
--     local win = hs.window.focusedWindow()
--     local app = win:application()
--     hs.layout.apply({{ app, win, nil, layout }})
-- end
-- 
-- function bindLayout(key, layout)
--     hs.hotkey.bind(hsModifier, key, function()
--         winLayout(layout)
--     end)
-- end
-- 
-- bindLayout("m", hs.layout.maximized)

hs.hotkey.bind(hsModifier, "m", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    -- If the window is full width, maximize it in vertical direction.
    if f.h == screen.h then
        f.x = screen.x
        f.w = screen.w
    end

    -- Always maximize in height.
    f.y = screen.y
    f.h = screen.h

    win:setFrame(f)
end)

hs.hotkey.bind(hsModifier, "j", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    f.x = screen.x
    f.w = screen.w * 0.4
    win:setFrame(f)
end)

hs.hotkey.bind(hsModifier, ";", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    f.x = screen.x  + screen.w * 0.4
    f.w = screen.w * 0.6
    win:setFrame(f)
end)

function vsplitHeight(screen)
    if (screen.h > 3000) then
        return screen.h / 3
    else
        return screen.h / 2
    end
end

hs.hotkey.bind(hsModifier, "k", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    f.h = vsplitHeight(screen)

    newY = math.min(f.y + f.h, screen.y + screen.h - f.h)
    f.y = newY

    win:setFrame(f)
end)

hs.hotkey.bind(hsModifier, "l", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()
    vh = vsplitHeight(screen)
    f.h = vh
    newY = math.max(f.y - f.h, screen.y)
    f.y = newY

    win:setFrame(f)
end)
