#/usr/bin/env bash

# A script to remind me of getting up every half hour. Add this string to your
# crontab:
#
#     0,30 * * * * /path/to/this/script/hourly-reminder.sh

script_dir=$(dirname "$0")
hours=$(date +%H)
minutes=$(date +%M)
title="$hours:$minutes"

if [[ $(uname -a) =~ Darwin ]]; then
    MACOS=true
fi

export DISPLAY=$(pgrep -fa xserverrc | cut -d " " -f 6)

if [ $(command -v task) ]; then
    notification="Pending: $(task status:pending count)"
fi

case $minutes in
     00)
         reminder=reminder
         ;;
     *)
         reminder=reminder-soft
esac

if [[ $MACOS ]]; then
    terminal-notifier -message "$notification" -title "$title" -sound Tink
else
    pactl play-sample $reminder
    notify-send "$title" "$notification"
fi
