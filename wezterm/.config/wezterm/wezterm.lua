local wezterm = require 'wezterm'

local config = wezterm.config_builder()

config.window_decorations = "RESIZE"
config.hide_tab_bar_if_only_one_tab = true
config.tab_bar_at_bottom = true

-- OS Specific Configuration
local is_linux = function()
  return wezterm.target_triple:find("linux") ~= nil
end
local is_darwin = function()
  return wezterm.target_triple:find("darwin") ~= nil
end

if is_darwin() then
  config.font = wezterm.font_with_fallback {
    'Menlo',
  }
  config.font_size = 13

  local mux = wezterm.mux

  wezterm.on("gui-startup", function(cmd)
    -- Pick the active screen to maximize into, there are also other options, see the docs.
    local active = wezterm.gui.screens().active
    local _, _, window = mux.spawn_window(cmd or {})
    window:gui_window():set_position(active.x, active.y)
    window:gui_window():set_inner_size(active.width, active.height)
  end)

elseif is_linux() then
  config.font = wezterm.font_with_fallback {
    "Hack"
  }
  config.font_size = 11
end


local act = wezterm.action
-- Split / Pane navigation bindings similar to my tmux bindings.
config.keys = {
  {
    key = '\'',
    mods = 'ALT',
    action = act.SplitPane {
      direction = 'Down',
      size = { Percent = 45 },
    }
  },
  {
    key = '\\',
    mods = 'ALT',
    action = act.SplitPane {
      direction = 'Right',
      size = { Percent = 45 },
    }
  },
  { key = 'o', mods = 'ALT', action = act.RotatePanes 'Clockwise' },
  { key = 'o', mods = 'ALT|SHIFT', action = act.RotatePanes 'CounterClockwise' },
  {
    key = 'h',
    mods = 'ALT',
    action = act.ActivatePaneDirection 'Left',
  },
  {
    key = 'l',
    mods = 'ALT',
    action = act.ActivatePaneDirection 'Right',
  },
  {
    key = 'k',
    mods = 'ALT',
    action = act.ActivatePaneDirection 'Up',
  },
  {
    key = 'j',
    mods = 'ALT',
    action = act.ActivatePaneDirection 'Down',
  },
  {
    key = 'h',
    mods = 'ALT|SHIFT',
    action = act.AdjustPaneSize {'Left', 1},
  },
  {
    key = 'l',
    mods = 'ALT|SHIFT',
    action = act.AdjustPaneSize {'Right', 1},
  },
  {
    key = 'k',
    mods = 'ALT|SHIFT',
    action = act.AdjustPaneSize {'Up', 1},
  },
  {
    key = 'j',
    mods = 'ALT|SHIFT',
    action = act.AdjustPaneSize {'Down', 1},
  },
  {
    key = 'f',
    mods = 'ALT|SHIFT',
    action = act.TogglePaneZoomState,
  },
  {
    key = 't',
    mods = 'ALT|SHIFT',
    action = act.SpawnCommandInNewTab { cwd = wezterm.home_dir }
  }
}

local function scheme_for_appearance(appearance)
  if appearance:find("Dark") then
    -- `CutiePro`
    -- `Bitmute (terminal.sexy)`
    return "Bitmute (terminal.sexy)"
  else
    -- `dayfox`
    -- `Ef-Cyprus`
    return "dayfox"
  end
end

-- Inactive Pane
config.inactive_pane_hsb = {
  -- Keep saturation same
  saturation = 1,
  -- Just reduce the brightness slightly
  brightness = 0.95,
}

wezterm.on("window-config-reloaded", function(window, _)
  local overrides = window:get_config_overrides() or {}
  local appearance = window:get_appearance()
  local scheme = scheme_for_appearance(appearance)
  if overrides.color_scheme ~= scheme then
    overrides.color_scheme = scheme
    window:set_config_overrides(overrides)
  end
end)

return config
