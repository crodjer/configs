#!/usr/bin/env sh
## Configure Touchpad

synclient TapButton1=0
synclient VertEdgeScroll=0, VertTwoFingerScroll=1, HorizTwoFingerScroll=1
synclient HorizScrollDelta=-104, VertScrollDelta=-80
synclient VertHysteresis=0, HorizHysteresis=0
synclient FingerLow=20, FingerHigh=25
synclient MinSpeed=0, MaxSpeed=1
synclient PalmDetect=1, PalmMinWidth=5, PalmMinZ=12
synclient EmulateTwoFingerMinZ=40, EmulateTwoFingerMinW=14
synclient CoastingSpeed=0 # , CoastingFriction=100
synclient TouchpadOff=0 # Completely disable touchpad.
# syndaemon -dKi 0.5
