#!/usr/bin/env bash
pkill trayer
sleep 0.1
trayer --edge top --SetDockType true --align right --height 14 \
       --widthtype request --tint 0x555555 --transparent true --alpha 0
