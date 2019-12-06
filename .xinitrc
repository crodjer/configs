#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

systemctl --user import-environment DISPLAY
~/.xsessionrc
~/.xsession
