#!/usr/bin/env bash

export XDG_RUNTIME_DIR="/run/user/$(id -u)"
paplay /usr/share/sounds/freedesktop/stereo/message.oga
printf "\a" # Broadcast alert

if [[ -n $@ ]]; then
    title=$1
    body="${@:2}"
    notify-send "$1" "$body"
fi
