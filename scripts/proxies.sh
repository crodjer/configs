#!/usr/bin/env bash

tor &>> /tmp/$USER-tor.log &
polipo &>> /tmp/$USER-polipo.log &

wait
