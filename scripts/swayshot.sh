#!/usr/bin/env bash

# Borrowed from: https://jolyonbrown.com/post/sway-screenshots/
filename="screenshot-`date +%F-%T`"
grim -g "$(slurp)" ~/downloads/screenshots/$filename.png
