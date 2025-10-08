#!/bin/sh

# This is to disable the FF safe mode key:
# https://github.com/Hammerspoon/hammerspoon/issues/514#issuecomment-972834362
launchctl setenv MOZ_DISABLE_SAFE_MODE_KEY 1
