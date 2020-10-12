-- Modifier to be used across Hammerspoon bindings.
local hsModifier = { "ctrl", "alt" }

-- The in-built Macbook display
local lcd = "LCD"

-- The attached external monitor
local monitor = "MONITOR"

-- List of apps and their screeen / binding, configuration
local appList = {
    iTerm2 = {
       screen = monitor,
       binding = "t",
       -- iTerm2 has a different appName and window name.
       appName = 'iTerm'
    },
    ["IntelliJ IDEA"] = {
       screen = monitor,
       binding = "e",
    },
    ["Brave Browser"] = {
       screen = lcd,
       binding = "b",
    },
    ["zoom.us"] = {
       screen = lcd,
       binding = "o"
    },
    Slack = {
       screen = lcd,
       binding = "s",
       autoHide = true
    },
    Postman = {
       screen = lcd,
       binding = "r",
       autoHide = true
    },
    Music = {
       screen = lcd,
       autoHide = true
    },
    Firefox = {
       screen = lcd,
       autoHide = true
    },
    Signal = { screen = lcd, autoHide = true },
    Finder = { screen = lcd },
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
spoon.ReloadConfiguration.watch_paths = { hs.configdir, "~/configs/.hammerspoon" }
spoon.ReloadConfiguration:start()

-- WindowScreenLeftAndRight: Shorcut to move windows through screens.
hs.loadSpoon("WindowScreenLeftAndRight")
spoon.WindowScreenLeftAndRight:bindHotkeys({
   screen_left = { hsModifier, "[" },
   screen_right= { hsModifier, "]" },
})

-- Seal: The awesome seal plugin, with pasteboard (pb) support.
hs.loadSpoon("Seal")
spoon.Seal:loadPlugins({ "apps", "pasteboard", "urlformats", "useractions" })
spoon.Seal.plugins.pasteboard.historySize = 100
spoon.Seal.plugins.useractions.actions = {
   ["Heimdall Jira"] = {
      url = "https://ifountain.atlassian.net/browse/HEIMDALL-${query}",
      keyword = "hj"
   },
   ["OG Support"] = {
      url = "https://ifountain.atlassian.net/browse/OGS-${query}",
      keyword = "ogs"
   }
}
spoon.Seal.plugins.urlformats:providersTable({
   hj = { name = "Heimdall Jira", url = "https://ifountain.atlassian.net/browse/HEIMDALL-%s" }
})
spoon.Seal:bindHotkeys({
    toggle = { {"cmd"}, "space" }
})
spoon.Seal:start()

-- Hide all applications
hs.hotkey.bind(hsModifier, "h", function()
   function hideApps()
      for _, app in pairs(hs.application.runningApplications()) do
         app:hide()
      end
   end

   hideApps()
   -- IDEA/Brave misbehave, so try hiding them once again.
   hs.timer.doAfter(0.1, hideApps)
end)

-- Show all applications
hs.hotkey.bind(hsModifier, "a", function()
   for _, app in pairs(hs.application.runningApplications()) do
      app:unhide()
   end
end)

-- Attach app bindings
for app, config in pairs(appList) do
    -- Bind the app 
    if config.binding ~= nil then
       hs.hotkey.bind(hsModifier, config.binding, function()
          hs.application.launchOrFocus(config.appName or app)
       end)
    end
end

function setAppLayout(app, config)
   if hs.application.find(app) == nil then
      return
   end

   -- Try to get the app's screen, otherwise default to LCD
   screen = hs.screen.find(config.screen) or hs.screen.find(lcd)
   layout = config.layout or hs.layout.maximized

   hs.layout.apply({
      [app] = { app, nil, screen, layout, nil, nil }
   })
end

-- Maximize the focussed window for the given app.
function maximizeApp(app, retry)
   window = app:focusedWindow()
   if window then
      window:maximize(0)
   elseif retry == nil then
      hs.timer.doAfter(5, function ()
         maximizeApp(app, true)
      end)
   end
end

-- Set layout as per config on screen changes.
function setLayout()
   for appName, config in pairs(appList) do
      setAppLayout(appName, config)
   end
end
screenWatcher = hs.screen.watcher.new(setLayout)
screenWatcher:start()

-- Handle activation/launch events for apps.
function handleAppEvent(appName, event, app, retry)
   config = appList[appName]
   if config == nil then
      return
   end

   -- Window is activated, maximise it.
   if event == hs.application.watcher.activated then
      maximizeApp(app)
   -- Window was launched, launch it with the correct layout / screen.
   elseif event == hs.application.watcher.launched then
      maximizeApp(app)
      setAppLayout(appName, config)
   -- Deactivate event, window should be auto-hidden
   elseif event == hs.application.watcher.deactivated and config.autoHide then
      local screenLCD = hs.screen.find(lcd)
      local screenMonitor = hs.screen.find(monitor) or screenLCD

      for _, window in pairs(app:allWindows()) do
         -- If any autohide app's window is on the monitor, hide the app.
         if window:screen() == screenMonitor then
            app:hide()
            break
         end
      end
   end
end

appWatcher = hs.application.watcher.new(handleAppEvent)
appWatcher:start()
