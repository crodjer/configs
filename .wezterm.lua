local wezterm = require 'wezterm'

local config = wezterm.config_builder()

config.window_decorations = "NONE"
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
  config.font_size = 13.5

  -- Split / Pane navigation bindings similar to my tmux bindings.
  local act = wezterm.action
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
  -- Mac's handle dark screen well through dimming.
  config.color_scheme = "Catppuccin Mocha"
end

if is_linux() then
  config.font = wezterm.font_with_fallback {
    "Hack"
  }
  -- On my linux machine, its actually eco friendly to use it in light mode
  -- but with low brightness.
  config.color_scheme = "Catppuccin Latte"
  config.font_size = 11
end


return config
