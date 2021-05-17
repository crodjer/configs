#!/usr/bin/env bash

ping 8.8.8.8 | grep --line-buffered -Eo '\d+\.\d+ ms'
