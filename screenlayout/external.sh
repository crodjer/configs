#!/bin/sh
xrandr --output HDMI1 --off --output LVDS1 --off --output DP1 --off --output VGA1 --off
xrandr --output VGA1 --mode 1920x1080 --pos 0x0 --rotate normal
