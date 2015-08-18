#/usr/bin/env bash

# A script to remind me of getting up every half hour. Add this string to your
# crontab:
#
#     0,30 * * * * /path/to/this/script/hourly-reminder.sh

script_dir=$(dirname "$0")

hours=$(date +%H)
minutes=$(date +%M)
title="$hours:$minutes"

if [[ $(ps -ef | grep -v grep | grep "xserverrc" | grep " :0 ") ]]; then
    export DISPLAY=:0
elif [[ $(ps -ef | grep -v grep | grep "xserverrc" | grep " :1 ") ]]; then
    export DISPLAY=:1
fi

if [ $(command -v task) ]; then
    notification="Pending: $(task status:pending count)"
fi

alert.sh $title $notification
