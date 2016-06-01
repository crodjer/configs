#!/usr/bin/env sh
## Configure Pulseaudio

start-pulseaudio-x11
pactl set-sink-volume 0 125%
pactl upload-sample /usr/share/sounds/freedesktop/stereo/complete.oga reminder
pactl upload-sample /usr/share/sounds/freedesktop/stereo/message-new-instant.oga reminder-soft
pactl upload-sample /usr/share/sounds/freedesktop/stereo/message.oga beep
pactl load-module module-x11-bell sample=beep display="$DISPLAY" > /dev/null
xset b 100
