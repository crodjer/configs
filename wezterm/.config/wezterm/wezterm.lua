local wezterm = require 'wezterm'

local config = wezterm.config_builder()

config.window_decorations = "RESIZE"
config.hide_tab_bar_if_only_one_tab = true

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
  config.font_size = 14

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
    action = act.SplitVertical { domain = 'CurrentPaneDomain' },
  },
  {
    key = '\\',
    mods = 'ALT',
    action = act.SplitHorizontal { domain = 'CurrentPaneDomain' },
  },
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
}

local function scheme_for_appearance(appearance)
  if appearance:find("Dark") then
    return "Catppuccin Mocha"
  else
    return "Catppuccin Latte"
  end
end

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
