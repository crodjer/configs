#!/usr/bin/env bash

set -e

host=$1

if [ -z $host ]; then
    self=true
    host=$(hostname)
fi

title() {
    printf $'\ek%s\e\\' "$1";
}

title $host

if [ -f "$HOME/.keys/$host.key" -a -z "$self" ]; then
    scp -q $HOME/.keys/$host.key $host:/tmp/
fi

if [ -z $self ]; then
    autossh -M 0 -t $host peek.sh
else
    tmux new -As $host
fi
