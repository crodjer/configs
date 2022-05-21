#!/usr/bin/env bash

script_file=$0
host_name=$1
safe_dir=${2:-/mnt/external/safe}
script_name=$(basename $script_file)

remote_target="/tmp"
remote_name="remote-unlock.sh"
remote_script="$remote_target/$remote_name"
remote_key="$remote_target/$host_name.key"
log_file=/tmp/$remote_name.log
on_unlock_script=$HOME/.local/bin/on-unlock.sh

if [ -z "$host_name" ]; then
    echo "Host name is required!"
    exit 1
fi

function run {
    if [ $script_name == $remote_name ]; then
        echo "[$(date -Iseconds)] Running: $@" &>> $log_file
        $@ &>> $log_file
    else
        echo "This should only be run as a remote script!"
        exit 1
    fi
}

if [ $script_name != "$remote_name" ]; then
    # Try reaching the remote host.
    PING_OUTPUT=$(timeout 1 ping -c 1 $host_name | grep -E 'time=[0-9.]+')
    if [ -z "$PING_OUTPUT" ]; then
        echo "Can't ping $host_name from $(hostname)!"
        exit 1
    fi

    scp -q $script_file $host_name:$remote_script || exit 1
    scp -q "$HOME/.keys/$host_name.key" $host_name:$remote_key || exit 1
    ssh $host_name -- $remote_script $host_name $safe_dir
    exit 0
fi

fscrypt_status=$(fscrypt status /)
remote_protector=$(echo "$fscrypt_status" | grep remote | cut -f 1 -d ' ')
login_protector=$(echo "$fscrypt_status" | grep login | cut -f 1 -d ' ')

unlocked () {
    local dir=$1
    # Not managed by fscrypt.
    if [ -z "$(fscrypt status $dir 2> /dev/null)" ]; then
        return
    fi

    test -n "$(fscrypt status $dir | grep 'Unlocked: Yes')"
}

unlock () {
    set -e

    local dir=$1
    # Not a directory!
    if [ ! -d $dir ]; then
        return;
    fi

    if unlocked $dir; then
        return;
    fi

    if [ -n "$remote_protector" -a -f "$remote_key" ]; then
        run fscrypt unlock $dir --unlock-with=/:$remote_protector --key=$remote_key
        return;
    fi

    # Is Home directory, try unlocking with password.
    if [ $1 == $HOME ]; then
        run fscrypt unlock $dir --unlock-with=/:$login_protector
    fi

    return 1;
}

unlock $HOME $remote_key
unlock $safe_dir $remote_key

run rm $remote_script
run rm $remote_key

if [ -x "$on_unlock_script" ]; then
    run $on_unlock_script
fi
