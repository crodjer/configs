-- Modifier to be used across Hammerspoon bindings.
local hsModifier = { "ctrl", "alt" }
local hsShift = { "ctrl", "alt", "shift" }
local logger = hs.logger.new('init.lua', 'info')

hs.window.animationDuration = 0

-- List of apps and their screeen / binding, configuration
local appList = {
    Alacritty = { binding = "t" },
    ["IntelliJ IDEA"] = { binding = "e" },
    ["Google Chrome"]= { binding = "c" },
    Obsidian = { binding = "n" },
    Notes = { binding = "i" },
    ["zoom.us"] = { binding = "o" },
    -- Postman = { binding = "r" },
    Slack = { binding = "s" },
    Peek = { binding = "p" },
    UTM = { binding = "u", bundleId = "com.utmapp.UTM" },
    -- Bitwarden = { binding = "w" },
    Signal = { binding = "g" },
    -- ["Firefox Developer Edition"] = { binding = "f" },
    -- VLC = { binding = "v" },
    -- Gmail = {
    --     binding = "i",
    --     bundleId = "com.google.Chrome.app.fmgjjmmmlfnkbppncabfkddbjimcfncm"
    -- },
    -- YouTube = {
    --     binding = "y",
    --     bundleId = "com.google.Chrome.app.agimnkijcaahngcdmfeangaknmldooml"
    -- },
    --["Prime Video"] = {
    --    binding = "=",
    --    bundleId = "com.google.Chrome.app.igpjbmoihojghddcmflmgeeadjkanlij"
    --},
    -- Messages = {
    --     binding = "8",
    --     bundleId = "com.google.Chrome.app.hpfldicfbfomlpcikngkocigghgafkph"
    -- },
    -- Snapdrop = {
    --     binding = ".",
    --     bundleId = "com.google.Chrome.app.ikpmlgdcejalmjnfbahhijemkcgljabf"
    -- },
    Hammerspoon = { binding = '9' },
    ["Yi Home"] = { binding = '0' },
}

function render(object)
    rendered = ""
    if type(object) == 'table' then
        for k,v in pairs(object) do
            rendered = rendered .. "\n" .. k .. " => " .. tostring(v)
        end
    else
        rendered = tostring(object)
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
spoon.ReloadConfiguration.watch_paths = { 
    hs.configdir .. "/init.lua",
    hs.configdir .. "/Spoons"
}
spoon.ReloadConfiguration:start()

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
spoon.Seal.plugins.pasteboard.saveHistory = no

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

hs.alert.defaultStyle.radius = 10
hs.alert.defaultStyle.atScreenEdge = 2
hs.alert.defaultStyle.fillColor = { white = 0, alpha = 0.6 }
hs.alert.defaultStyle.textSize = 15
hs.alert.defaultStyle.fadeInDuration = 0.1
hs.alert.defaultStyle.fadeOutDuration = 0.1

function activateApp(application, config)
    if not application then
        return
    end

    windows = application:allWindows()
    focusedWindow = hs.window.focusedWindow()
    window = windows[1]

    if focusedWindow and focusedWindow:application() == application then
        hs.eventtap.keyStroke({"cmd"}, "`")
    elseif window then
        if (window:isMinimized()) then
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

function launchApp(app, config)
    if config.bundleId then
        hs.application.launchOrFocusByBundleID(config.bundleId)
    else
        hs.application.launchOrFocus(app)
    end
end

appNotRunningAlertId = nil
lastNotRunningApp = nil

-- Attach app bindings
for app, config in pairs(appList) do
    -- Bind the app
    if config.binding ~= nil then
        hs.hotkey.bind(hsModifier, config.binding, function()
            local application = hs.application.get(config.bundleId or app)

            if application then
                lastNotRunningApp = nil
                activateApp(application, config)
            elseif config.autoLaunch or lastNotRunningApp == app then
                lastNotRunningApp = nil
                launchApp(app, config)
                activateApp(application, config)
            else
                hs.alert.closeSpecific(appNotRunningAlertId, 0)
                appNotRunningAlertId = hs.alert(app .. " not running!", nil, nil, 1)
                lastNotRunningApp = app
            end
        end)
    end
end

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

hs.hotkey.bind(hsModifier, "h", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    f.x = screen.x
    f.w = screen.w * 0.42
    win:setFrame(f)
end)

hs.hotkey.bind(hsModifier, "l", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    f.x = screen.x  + screen.w * 0.42
    f.w = screen.w * 0.58
    win:setFrame(f)
end)

function vsplitBreadth(screen)
    if (screen.h > 3000) then
        return screen.h / 3
    else
        return screen.h / 2
    end
end

function vsplitHeight(screen)
    if (screen.h > 3000) then
        return screen.h / 3
    else
        return screen.h / 2
    end
end

hs.hotkey.bind(hsModifier, "j", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    f.h = vsplitHeight(screen)

    newY = math.min(f.y + f.h, screen.y + screen.h - f.h)
    f.y = newY

    win:setFrame(f)
end)

hs.hotkey.bind(hsModifier, "k", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()
    vh = vsplitHeight(screen)
    f.h = vh
    newY = math.max(f.y - f.h, screen.y)
    f.y = newY

    win:setFrame(f)
end)

hs.hotkey.bind(hsShift, "w", function ()
    local task = hs.task.new("/Users/rjain3/.local/bin/stew.sh", function (code, stdout, stderr)
        hs.eventtap.keyStrokes(stdout:gsub("[\n\r]", ""))
    end)
    task:start()
end)

for space_index=1, 7 do
    function selectSpace()
        current_spaces = hs.spaces.allSpaces()[hs.spaces.spaceDisplay(hs.spaces.focusedSpace())]
        space = current_spaces[space_index]
        if space then
            hs.spaces.gotoSpace(space)
        end
    end
    hs.hotkey.bind(hsModifier, tostring(space_index), selectSpace)
end

-- Mic Control
-- talkingAlert = nil
-- unMuteAlertDuration = 600
-- 
-- function unMuteAlert()
--     hs.alert.closeSpecific(talkingAlert, 0)
--     talkingAlert = hs.alert("🗣️", {
--         textSize = 32,
--         fillColor = { white = 0.8, alpha = 0.6 },
--         fadeInDuration = 0,
--         fadeOutDuration = 0
--     }, unMuteAlertDuration)
-- end

-- function unMuteMic()
--     unMuteAlert()
--     hs.timer.doUntil(function ()
--          return hs.audiodevice.defaultInputDevice():muted()
--     end, unMuteAlert, unMuteAlertDuration / 2)
-- 
--     for _, device in pairs(hs.audiodevice.allInputDevices()) do
--         device:setMuted(false)
--     end
-- end
-- function muteMic()
--     hs.alert.closeSpecific(talkingAlert, 0)
--     for _, device in pairs(hs.audiodevice.allInputDevices()) do
--         device:setMuted(true)
--     end
-- end
-- 
-- -- Push to talk!
-- hs.hotkey.bind({'ctrl'}, 'space', unMuteMic, muteMic)
-- -- Toggle the Mic!
-- -- `Ctrl-Enter` for PTT / Mute and `Ctrl+Alt+Enter` for just
-- -- unmute does work. But one of my mice doesn't support tapping
-- -- into key up/down events and hence can't PTT.
-- -- So, Toggle with `Ctrl+Alt+Enter` allows me to use that Mouse
-- -- as a mic control as well.
-- hs.hotkey.bind(hsModifier, 'space', function ()
--     if(hs.audiodevice.defaultInputDevice():muted()) then
--         unMuteMic()
--     else
--         muteMic()
--     end
-- end)
-- Mute by default!
-- muteMic()
