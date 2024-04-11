#!/usr/bin/env bash

interface=$(ip route show 0.0.0.0/0 | awk '{ print $5 }')

if [ "$interface" ]; then
    echo -e "$interface |"
fi
