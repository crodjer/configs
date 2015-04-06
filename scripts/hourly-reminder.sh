#/usr/bin/env bash

# A script to remind me of getting up every half hour. Add this string to your
# crontab:
#
#     0,30 * * * * /path/to/this/script/hourly-reminder.sh

hours=$(date +%H)
minutes=$(date +%M)
# notification="$hours:$minutes"
title="$hours:$minutes"

allow_beep="true"

jacked.sh && {
    unset allow_beep
}

case $minutes in
     30)
         beep_args="-f 900 -l 100"
         ;;
     00)
         beep_args="-f 250 -l 400"
         ;;
     *)
         ;;
esac

export DISPLAY=:0
notify-send "$title" # "$notification"

if [[ $beep_args && $allow_beep ]]; then
    beep $beep_args
fi
