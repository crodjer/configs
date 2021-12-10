#!/usr/bin/env bash

set -e

host=$1
safe_dir=${2:-/mnt/external/safe}
parent_dir=$(dirname $0)

if [ -z $host ]; then
    self=true
    host=$(hostname)
fi

title() {
    printf $'\ek%s\e\\' "$1";
}

title $host

if [ -z $self ]; then
    $parent_dir/unlock-remote.sh $host $safe_dir
    autossh -M 0 -t $host -- tmux -u new -As "$host"
else
    tmux new -As $host
fi
