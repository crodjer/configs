#/usr/bin/env bash

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
EXCLUDE_FROM=$DIR/../.backup-excludes.txt
TARGET_HOME="$1"
TARGET_ROOT="$2"
EXECUTE="$3"

rsync_opts="-valrhpoxt --one-file-system --exclude-from=$EXCLUDE_FROM"

if [[ ! "$EXECUTE" == "-x" ]]; then
    rsync_opts="$rsync_opts -n"
fi

backup () {
    rsync $rsync_opts -- $1 $2
}

if [[ "$TARGET_ROOT" != /mnt/* || -z "$TARGET_HOME" ]]; then
    echo "Require both target home and target root!" >&2
    exit 1
fi

backup /home/ $TARGET_HOME
backup / $TARGET_ROOT
