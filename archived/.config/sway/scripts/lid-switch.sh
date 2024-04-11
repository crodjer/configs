#!/bin/bash

if cat /proc/acpi/button/lid/LID*/state | grep closed &> /dev/null; then
    swaymsg output eDP-1 disable
else
    swaymsg output eDP-1 enable
fi
