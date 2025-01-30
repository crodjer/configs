-- Modifier to be used across Hammerspoon bindings.
local hsModifier = { "ctrl", "alt" }
local hsShift = { "ctrl", "alt", "shift" }
-- local logger = hs.logger.new('init.lua', 'info')

hs.window.animationDuration = 0

-- List of apps and their screeen / binding, configuration
local appList = {
    [ "Google Chrome" ] = { binding = "c" },
    WezTerm = { binding = "e", autoLaunch = true },
    Slack = { binding = "s" },
    Slab = { binding = "n" },
    UTM = { binding = "u", bundleId = "com.utmapp.UTM" },
    Bitwarden = { binding = "p" },
    Signal = { binding = "g", mayHide = true },
    WezTerm = { binding = "t" },
    Firefox = { binding = "f", mayHide = true  },
    DuckDuckGo = { binding = "d", mayHide = true  },
    Hammerspoon = { binding = '9' }
}

function Render(object)
    local rendered = ""
    if type(object) == 'table' then
        for k,v in pairs(object) do
            rendered = rendered .. "\n" .. k .. " => " .. tostring(v)
        end
    else
        rendered = tostring(object)
    end
    print(rendered)
end

hs.loadSpoon("EmmyLua")

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

----------------------------
-- App and Window Management
----------------------------
local InvalidOpNotification = nil
function ActivateApp(application, config)
    if not application then
        return
    end

    local windows = application:allWindows()
    local focusedWindow = hs.window.focusedWindow()
    local window = windows[1]

    if application:isHidden() and config.mayHide then
        hs.alert.closeSpecific(InvalidOpNotification, 0)
        InvalidOpNotification = hs.alert(" ! ", nil, nil, 1)
        return
    else
        InvalidOpNotification = nil
    end


    if focusedWindow and focusedWindow:application() == application then
        hs.eventtap.keyStroke({"cmd"}, "`")
    elseif window then
        if window:isMinimized() then
            hs.application.launchOrFocus(application:name())
        else
            window:focus()
        end
    else
        application:activate()
        if not application:allWindows()[1] then
            hs.application.launchOrFocus(application:name())
        end
    end
end

function LaunchApp(app, config)
    if config.bundleId then
        hs.application.launchOrFocusByBundleID(config.bundleId)
    else
        hs.application.launchOrFocus(app)
    end
end

local AppNotRunningAlertId = nil
local LastNotRunningApp = nil

-- Attach app bindings
for app, config in pairs(appList) do
    -- Bind the app
    if config.binding ~= nil then
        hs.hotkey.bind(hsModifier, config.binding, function()
            local application = hs.application.get(config.bundleId or app)

            if application then
                LastNotRunningApp = nil
                ActivateApp(application, config)
            elseif config.autoLaunch or LastNotRunningApp == app then
                LastNotRunningApp = nil
                LaunchApp(app, config)
                ActivateApp(application, config)
            else
                hs.alert.closeSpecific(AppNotRunningAlertId, 0)
                AppNotRunningAlertId = hs.alert(app .. " not running!", nil, nil, 1)
                LastNotRunningApp = app
            end
        end)
    end
end

-- hs.window Configuration
hs.window.animationDuration = 0
hs.window.setFrameCorrectness = true

hs.hotkey.bind(hsModifier, "m", function()
    local win = hs.window.focusedWindow()
    if not win then
        return
    end
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

hs.hotkey.bind(hsModifier, "h", function()
    local win = hs.window.focusedWindow()
    if not win then
        return
    end
    local f = win:frame()
    local screen = win:screen():frame()

    f.x = screen.x
    f.w = screen.w * 0.42
    win:setFrame(f)
end)

hs.hotkey.bind(hsModifier, "l", function()
    local win = hs.window.focusedWindow()
    if not win then
        return
    end
    local f = win:frame()
    local screen = win:screen():frame()

    f.x = screen.x  + screen.w * 0.42
    f.w = screen.w * 0.58
    win:setFrame(f)
end)

function VsplitBreadth(screen)
    if screen.h > 3000 then
        return screen.h / 3
    else
        return screen.h / 2
    end
end

function VsplitHeight(screen)
    if screen.h > 3000 then
        return screen.h / 3
    else
        return screen.h / 2
    end
end

hs.hotkey.bind(hsModifier, "j", function()
    local win = hs.window.focusedWindow()
    if not win then
        return
    end
    local f = win:frame()
    local screen = win:screen():frame()

    f.h = VsplitHeight(screen)

    local newY = math.min(f.y + f.h, screen.y + screen.h - f.h)
    f.y = newY

    win:setFrame(f)
end)

hs.hotkey.bind(hsModifier, "k", function()
    local win = hs.window.focusedWindow()
    if not win then
        return
    end
    local f = win:frame()
    local screen = win:screen():frame()
    local vh = VsplitHeight(screen)
    f.h = vh
    local newY = math.max(f.y - f.h, screen.y)
    f.y = newY

    win:setFrame(f)
end)

hs.hotkey.bind(hsShift, "w", function ()
    local task = hs.task.new("/Users/rohan/.local/bin/stew.sh", function (_, stdout, _)
        hs.eventtap.keyStrokes(stdout:gsub("[\n\r]", ""))
    end)
    task:start()
end)

----------------------------
-- Spoons
----------------------------
-- Seal: The awesome seal plugin, with pasteboard (pb) support.
hs.loadSpoon("Seal")
spoon.Seal:loadPlugins({ "apps", "pasteboard" })
spoon.Seal.plugins.apps.appSearchPaths = {
   "/Applications",
   "/System/Applications",
   "~/Applications",
   "/System/Library/PreferencePanes",
   "/Library/PreferencePanes",
   "/System/Library/CoreServices/Applications",
}
spoon.Seal.plugins.apps:restart()
spoon.Seal.plugins.pasteboard.historySize = 100
spoon.Seal.plugins.pasteboard.saveHistory = true

spoon.Seal:bindHotkeys({
    toggle = { {"cmd"}, "space" }
})
spoon.Seal:start()

-- Cherry
local Cherry = hs.loadSpoon("Cherry")
Cherry:bindHotkeys({
  start = { hsShift, "t" }
})

-- Caffeine
local Caffeine= hs.loadSpoon("Caffeine")
Caffeine:start()

-- Switcher
local Switcher = hs.window.switcher
Switcher.ui.titleBackgroundColor = {0, 0, 0, 0}
Switcher.ui.fontName = 'Verdana'
Switcher.ui.textSize = 12
Switcher.ui.showThumbnails = false
Switcher.ui.showSelectedThumbnail = false

hs.alert.defaultStyle.radius = 10
hs.alert.defaultStyle.atScreenEdge = 2
hs.alert.defaultStyle.fillColor = { white = 0, alpha = 0.6 }
hs.alert.defaultStyle.textSize = 15
hs.alert.defaultStyle.fadeInDuration = 0.1
hs.alert.defaultStyle.fadeOutDuration = 0.1
