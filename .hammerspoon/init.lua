-- Modifier to be used across Hammerspoon bindings.
local hsModifier = { "ctrl", "alt" }
local hsShift = { "ctrl", "alt", "shift" }
local logger = hs.logger.new('init.lua', 'info')

hs.window.animationDuration = 0

-- List of apps and their screeen / binding, configuration
local appList = {
    Alacritty = { binding = "t" },
    ["Android Studio"] = { binding = "a" },
    ["IntelliJ IDEA"] = { binding = "e" },
    Slack = { binding = "s" },
    Postman = { binding = "r" },
    Bitwarden = { binding = "w" },
    Signal = { binding = "g" },
    ["Firefox Developer Edition"] = { binding = "f" },
    UTM = { binding = "u", bundleId = "com.utmapp.UTM" },
    Peek = { binding = "p" },
    ["Google Chrome"]= { binding = "c" },
    VLC = { binding = "v" },
    Obsidian = { binding = "n" },
    ["zoom.us"] = { binding = "o" },
    Hammerspoon = { binding = '2' },
    Zoom = {
        binding = "3",
        bundleId = "com.google.Chrome.app.gbmplfifepjenigdepeahbecfkcalfhg"
    },
    YouTube = {
        binding = "y",
        autoLaunch = false,
        bundleId = "com.google.Chrome.app.agimnkijcaahngcdmfeangaknmldooml"
    },
    ["Prime Video"] = {
        binding = "=",
        autoLaunch = false,
        bundleId = "com.google.Chrome.app.igpjbmoihojghddcmflmgeeadjkanlij"
    },
    Messages = {
        binding = "0",
        autoLaunch = false,
        bundleId = "com.google.Chrome.app.hpfldicfbfomlpcikngkocigghgafkph"
    },
    Snapdrop = {
        binding = ".",
        autoLaunch = true,
        bundleId = "com.google.Chrome.app.ikpmlgdcejalmjnfbahhijemkcgljabf"
    }
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
spoon.Seal.plugins.pasteboard.saveHistory = yes

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
hs.alert.defaultStyle.fillColor = { white = 0, alpha = 0.6 }
hs.alert.defaultStyle.textSize = 15
hs.alert.defaultStyle.fadeInDuration = 0.1
hs.alert.defaultStyle.fadeOutDuration = 0.1

noWindowAlertId = nil
lastNotInWorkspaceApp = nil

function activateApp(application, config)
    if not application then
        return
    end

    windows = application:allWindows()
    focusedWindow = hs.window.focusedWindow()
    window = windows[1]

    if focusedWindow and focusedWindow:application() == application then
        lastNotInWorkspaceApp = nil
        hs.eventtap.keyStroke({"cmd"}, "`")
    elseif window then
        lastNotInWorkspaceApp = nil
        window:focus()
    elseif lastNotInWorkspaceApp == application:name() then
        lastNotInWorkspaceApp = nil
        application:activate()
    else
        hs.alert.closeSpecific(noWindowAlertId, 0)
        noWindowAlertId = hs.alert(application:name() .. " not in in current workspace!", nil, nil, 1)
        lastNotInWorkspaceApp = application:name()
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

hs.hotkey.bind(hsModifier, "j", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    f.x = screen.x
    f.w = screen.w * 0.42
    win:setFrame(f)
end)

hs.hotkey.bind(hsModifier, ";", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen():frame()

    f.x = screen.x  + screen.w * 0.42
    f.w = screen.w * 0.58
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

hs.hotkey.bind(hsShift, "w", function ()
    local task = hs.task.new("/Users/rjain3/.local/bin/stew.sh", function (code, stdout, stderr)
        hs.eventtap.keyStrokes(stdout)
    end)
    task:start()
end)

-- Mic Control
talkingAlert = nil
unMuteAlertDuration = 600

function unMuteAlert()
    hs.alert.closeSpecific(talkingAlert, 0)
    talkingAlert = hs.alert("🗣️", {
        textSize = 32,
        fillColor = { white = 0.8, alpha = 0.6 },
        fadeInDuration = 0,
        fadeOutDuration = 0
    }, unMuteAlertDuration)
end

function unMuteMic()
    unMuteAlert()
    hs.timer.doUntil(function ()
         return hs.audiodevice.defaultInputDevice():muted()
    end, unMuteAlert, unMuteAlertDuration / 2)

    for _, device in pairs(hs.audiodevice.allInputDevices()) do
        device:setMuted(false)
    end
end
function muteMic()
    hs.alert.closeSpecific(talkingAlert, 0)
    for _, device in pairs(hs.audiodevice.allInputDevices()) do
        device:setMuted(true)
    end
end

-- Push to talk!
hs.hotkey.bind({'ctrl'}, 'space', unMuteMic, muteMic)
-- Toggle the Mic!
-- `Ctrl-Enter` for PTT / Mute and `Ctrl+Alt+Enter` for just
-- unmute does work. But one of my mice doesn't support tapping
-- into key up/down events and hence can't PTT.
-- So, Toggle with `Ctrl+Alt+Enter` allows me to use that Mouse
-- as a mic control as well.
hs.hotkey.bind(hsModifier, 'space', function ()
    if(hs.audiodevice.defaultInputDevice():muted()) then
        unMuteMic()
    else
        muteMic()
    end
end)
-- Mute by default!
muteMic()
