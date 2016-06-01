#!/usr/bin/env sh
## Configure Keyboard

setxkbmap -option ctrl:nocaps
xmodmap -e "keycode 107 = Super_R NoSymbol Super_R"
xmodmap -e "keycode 135 = Super_R NoSymbol Super_R"
