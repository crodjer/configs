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
HOME_DIR=${HOME_DIR:-/home/rohan}

if [ -z "$SELF" ]; then
    ssh $DEVICE 'test -d /home/rohan/.ssh' || {
        log "Home directry not accessible on $DEVICE!"
            exit 1
        }
fi

if [ -z "$SELF" ]; then
    EXTRA_ARGS=$(ssh $DEVICE -- cat $HOME_DIR/.backup-rsync-params 2> /dev/null | tr '\n' ' ' || echo "" )
else
    EXTRA_ARGS=$(cat $HOME_DIR/.backup-rsync-params 2> /dev/null | tr '\n' ' ' || echo "" )
fi

EXCLUDES_FILE=$(mktemp -u --suffix=-rsync-backup-excludes)

# Generally excluded paths.
cat > $EXCLUDES_FILE <<- EOM
/dev/*
/proc/*
/sys/*
/tmp/*
/run/*
/mnt/*
/media/*
/var/log/*
/var/tmp/*
/swapfile
/lost+found
/root/.cache
/.snapshots
/var/lib/flatpak/runtime/*
/var/lib/flatpak/repo/*
/var/lib/flatpak/.removed/*
$HOME_DIR/.cache/*
$HOME_DIR/.cargo/git/*
$HOME_DIR/.cargo/registry/*
$HOME_DIR/.rustup/toolchains/*
EOM

sudo rsync -aAXH \
    --progress \
    --delete \
    --delete-excluded \
    --exclude-from=$EXCLUDES_FILE \
    $EXTRA_ARGS \
    $SOURCE $TARGET

rm $EXCLUDES_FILE
