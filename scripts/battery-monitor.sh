#!/usr/bin/env sh

acpi -b  | cut -d ' ' -f 3,4 | sed 's/[,%]//g' | {
    read -r status capacity


    if [ "$status" = Discharging -a "$capacity" -le 5 ]; then
        # Set enviornment so that `notify-send` works.
        _DBUS_ENV="$(egrep -z DBUS_SESSION_BUS_ADDRESS /proc/$(pgrep -u rohan dbus | head -1)/environ)"
        export DISPLAY=:0
        eval "export $_DBUS_ENV"

        sudo -u rohan /usr/bin/notify-send --urgency critical \
            "⚠ Battery Low!" \
            "Capcity: $capacity%. Going to sleep in 15 seconds."
        logger "Critical battery threshold"
        sleep 15
        systemctl hybrid-sleep
    fi
}


