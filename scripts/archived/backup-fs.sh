#/usr/bin/env bash

dir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
exclude_from=$dir/../.backup-excludes.txt
source="$1"
target="${2%/}"
status=0

rsync_opts="-valrhpoxt --one-file-system --exclude-from=$exclude_from ${@:2}"

if [[ "$source" != /* ]]; then
    echo "Please provide the source's absolute path" >&2
    exit 1
fi

if [[ "$target" != /mnt/* ]]; then
    echo "Target partition path should start with /mnt" >&2
    exit 1
fi
df | grep $target > /dev/null || {
    echo "Target should be a mounted partition" >&2
    exit 1
}

rsync $rsync_opts -- $source $target
