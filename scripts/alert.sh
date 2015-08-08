#!/usr/bin/env bash
printf "\a"
paplay /usr/share/sounds/freedesktop/stereo/message.oga

if [[ -n $@ ]]; then
    title=$1
    body="${@:2}"
    notify-send "$1" "$body"
fi
