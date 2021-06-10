#!/usr/bin/env sh

## Add to cron to ensure it is run every minute. Eg:
# * * * * * sudo /home/rohan/configs/scripts/battery-monitor.sh

user_name="rohan"

acpi -b  | cut -d ' ' -f 3,4 | sed 's/[,%]//g' | {
    read -r status capacity

    if [ "$status" = Discharging -a "$capacity" -le 10 ]; then
        sudo -u $user_name \
            DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/$(id -u $user_name)/bus \
            DISPLAY=:0 \
            /usr/bin/notify-send --urgency critical \
            "⚠ Battery Low!" \
            "Capcity: $capacity%. Going to sleep in 30 seconds."

        logger "Critical battery threshold"
        sleep 30
        systemctl suspend
    fi
}
