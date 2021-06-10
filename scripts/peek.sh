#!/usr/bin/env bash

host=$(cat /etc/hostname)
fscrypt_status=$(fscrypt status /)
remote_protector=$(echo $fscrypt_status | grep remote | cut -f 1 -d ' ')
login_protector=$(echo $fscrypt_status | grep login | cut -f 1 -d ' ')
log_file=/tmp/remote-login.log
safe_dir=/mnt/external/safe

if [ -n "$remote_protector" -a -f "/tmp/$host.key" ]; then
    fscrypt unlock $HOME --unlock-with=/:$remote_protector--key=/tmp/$host.key &> $log_file
    rm /tmp/$host.key
else
    fscrypt unlock $HOME &> $log_file
fi

if [ -n "$remote_protector" -a -d "$safe_dir" ]; then
    fscrypt unlock "$safe_dir" --unlock-with=/:$remote_protector--key=$HOME/.keys/ &> $log_file
fi

tmux -u new -As "$host"
