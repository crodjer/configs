#!/usr/bin/env bash

pin_ctls=$(grep 'Pin-ctls: 0x40: OUT'  /proc/asound/card0/codec#0 | wc -l)

if [ "$pin_ctls" == "2" ]; then
    exit 1
else
    exit 0
fi
