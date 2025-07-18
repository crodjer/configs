#!/usr/bin/env bash

set -e

HOME=/var/root sudo bash -c 'nix-channel --update && darwin-rebuild switch'
brew upgrade
pipx upgrade-all
cargo install-update --all
