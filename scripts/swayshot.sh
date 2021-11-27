#!/usr/bin/env bash

# Borrowed from: https://jolyonbrown.com/post/sway-screenshots/
filename="screenshot-`date +%F-%T`"
mkdir -p ~/pictures/screenshots
grim -g "$(slurp)" ~/pictures/screenshots/$filename.png
