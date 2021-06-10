#!/usr/bin/env bash

ping 1.1.1.1 | grep --line-buffered -Eo '[.0-9]+ ms'
