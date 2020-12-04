#!/usr/bin/env bash

SANDBOX_CONFIG=$HOME/.xmonad/cabal.sandbox.config

if [ -e $SANDBOX_CONFIG ]; then
    cabal --sandbox-config-file=$SANDBOX_CONFIG  exec xmonad -- $@
else
    xmonad $@
fi
