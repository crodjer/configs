#!/usr/bin/env bash
printf "\a"
paplay /usr/share/sounds/freedesktop/stereo/message.oga

if [[ -n $@ ]]; then
    notify-send "$1" "$2"
fi
