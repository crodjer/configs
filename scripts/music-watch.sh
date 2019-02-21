#!/usr/bin/env bash

dbus-monitor --session "type='signal',interface='org.gnome.ScreenSaver'" | while read x; do
    case "$x" in
      *"boolean true"*)
          rhythmbox-client --no-start --pause
          ;;
      *"boolean false"*)
          if jacked.sh; then
              rhythmbox-client --no-start --play
          fi
          ;;
    esac
  done &

acpi_listen | while read x; do
    if [[ "$x" =~ "HEADPHONE unplug" ]]; then
        rhythmbox-client --no-start --pause
    fi
done &

wait
