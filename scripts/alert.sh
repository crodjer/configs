#!/usr/bin/env bash

pactl play-sample beep

if [[ -n $@ ]]; then
    title=$1
    body="${@:2}"
    notify-send "$1" "$body"
fi
