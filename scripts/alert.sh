#!/usr/bin/env bash
paplay /usr/share/sounds/freedesktop/stereo/message.oga
printf "\a"

if [[ -n $@ ]]; then
    title=$1
    body="${@:2}"
    notify-send "$1" "$body"
fi
