#!/usr/bin/env bash
set -e

DEVICE=$1

function log() {
    echo "[$(date -Iseconds)][${DEVICE:-unknown}]" $@
}

if [ -z "$DEVICE" ]; then
    SOURCE="/"
    DEVICE=$(hostname)
    SELF=true
else
    SOURCE="$DEVICE:/"
fi

log "Backing up $DEVICE!"

TARGET=${TARGET:-/mnt/external/safe/backup/$DEVICE/}

if [ -z "$SELF" ]; then
    ssh $DEVICE 'test -d /home/rohan/.ssh' || {
        log "Home directry not accessible on $DEVICE!"
            exit 1
        }
fi

rsync -aAXH \
    --progress \
    --delete \
    --exclude={"/dev/*","/proc/*","/sys/*","/tmp/*","/run/*","/mnt/*","/media/*","/var/log/*","/var/tmp/*","/swapfile","/lost+found"} \
    $SOURCE $TARGET
