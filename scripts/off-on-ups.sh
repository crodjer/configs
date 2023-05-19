#!/usr/bin/env bash

log() {
    echo "[$(date -Iseconds)]" $@ >&2
}

usage()
{
cat << EOF
usage: $0 <device> <device>...

Safely shut down the system when the power at home is off and the computer is
running on the UPS.
EOF
}

SHUTDOWN=true

GATEWAY=$1
shift
DEVICES=($@)

if [ -z "${DEVICES[0]}" ]; then
    log "At least 1 device to test with is required!"
    usage
    exit 1
fi

is_online() {
    ip=$1
    if ping -c 3 -i 0.2 -w 1 $1 >/dev/null 2>&1
    then
        # log $1 is online!
        return 0
    else
        log $1 is offline!
        return 1
    fi
}

if ! is_online $GATEWAY; then
    log "Gateway doesn't match the expected IP"
    exit
fi

for device in "${DEVICES[@]}"; do
    echo $device
    if is_online $device; then
        SHUTDOWN=false
    fi
done

if $SHUTDOWN; then
    log "Power's likely out! Shutting down."
    # sudo poweroff
fi
