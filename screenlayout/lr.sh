#!/bin/sh
xrandr --output HDMI1 --off --output DP1 --off  --output VGA1 --off --output LVDS1 --off
xrandr --output LVDS1 --mode 1366x768 --pos 1920x312 --rotate normal --output VGA1 --mode 1920x1080 --pos 0x0
