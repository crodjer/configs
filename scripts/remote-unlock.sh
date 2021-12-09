#!/usr/bin/env bash

host=$(cat /etc/hostname)
fscrypt_status=$(fscrypt status /)
remote_protector=$(echo "$fscrypt_status" | grep remote | cut -f 1 -d ' ')
login_protector=$(echo "$fscrypt_status" | grep login | cut -f 1 -d ' ')
log_file=/tmp/remote-unlock.log
safe_dir=/mnt/external/safe
key="/tmp/$host.key"

function run {
  $@ &>> $log_file
}

unlocked () {
    # Not managed by fscrypt.
    if [ -z "$(fscrypt status $1 2> /dev/null)" ]; then
        return
    fi

    test -n "$(fscrypt status $1 | grep 'Unlocked: Yes')"
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


    if [ -n "$remote_protector" -a -f "$key" ]; then
        run fscrypt unlock $dir --unlock-with=/:$remote_protector --key=$key
        return;
    fi

    # Is Home directory, try unlocking with password.
    if [ $1 == $HOME ]; then
        run fscrypt unlock $dir --unlock-with=/:$login_protector
    fi

    return 1;
}

unlock $HOME $key
unlock $safe_dir $key
run rm -f $key

if [ -n "$(command -v on-unlock.sh)" ]; then
    run on-unlock.sh
fi
