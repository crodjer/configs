#/usr/bin/env bash

# A script to remind me of getting up every half hour. Add this string to your
# crontab:
#
#     0,30 * * * * /path/to/this/script/hourly-reminder.sh

title="Get up!"

hours=$(date +%H)
minutes=$(date +%M)

case $minutes in
     30)
         notification="Its half past $hours hours."
         mpc pause
         ;;
     00)
         notification="Its $hours hours."
         mpc pause
         ;;
     *)
         notification="Its $minutes past $hours hours."
         ;;
esac

DISPLAY=:0 notify-send "$title" "$notification"
