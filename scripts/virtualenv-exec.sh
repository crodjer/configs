#!/usr/bin/env bash

VIRTUALENV=$1
EXECUTABLE=$2

if [ $# -le  2 ]; then
    echo "Usage: $0 <virtualenv-path> <executable> [executable-args]"
    exit 1
fi

source $VIRTUALENV/bin/activate
$EXECUTABLE ${@:3}
