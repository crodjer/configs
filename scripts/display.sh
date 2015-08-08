#!/usr/bin/env bash

SCRIPTS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/" && pwd )"

SCREENS=0

# See if HDMI external monitor is present at VGA
xrandr | grep VGA | grep " connected " &> /dev/null
if [ $? -eq 0 ]; then
    SCREENS=$((1+SCREENS))
fi

# See if external monitor is present at HDMI
xrandr | grep HDMI | grep " connected " &> /dev/null
if [ $? -eq 0 ]; then
    HDMI_CONNECTED=true
    SCREENS=$((2+SCREENS))
fi

# Run this for reseting the looks of URxvt etc.
xrdb -merge ~/.Xresources

case $SCREENS in
    0) # Only laptop
        xrandr --output HDMI1 --off --output VGA1 --off
        xrandr --output LVDS1 --auto
    ;;
    1) # VGA + LAPTOP
        xrandr --output HDMI1 --off
        # xrandr --output  LVDS1 --auto --output VGA1 --auto --right-of LVDS1
        # xrandr --output VGA1 --auto --pos 0x0 --output LVDS1 --auto --pos 1920x312 --primary
        xrandr --output LVDS1 --auto --pos 0x312 --primary --output VGA1 --auto --pos 1366x0
    ;;
    2) # HDMI + LAPTOP
        xrandr --output VGA1 --off
        xrandr --output HDMI1 --auto --pos 0x0 --output LVDS1 --auto --pos 1920x312 --primary
    ;;
    3) # HDMI + VGA
        xrandr --output LVDS1 --off
        xrandr --output VGA1 --auto --pos 0x0 --output HDMI1 --auto --pos 1920x0 --primary
        hdmi_urxvt_fix
    ;;
esac

if [[ "$HDMI_CONNECTED" ]]; then
xrdb -merge <<EOF
URxvt*letterSpace:0
EOF
fi


# Reset my wallpaper
bash $HOME/.fehbg

# Rest trayer location
# mytrayer
