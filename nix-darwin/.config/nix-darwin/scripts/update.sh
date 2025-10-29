#!/usr/bin/env bash

set -e

HOME=/var/root sudo nix-channel --update
HOME=/var/root sudo darwin-rebuild switch
brew upgrade
pipx upgrade-all
cargo install-update --all

if [ -d /opt/homebrew/opt/socket_vmnet/ ]; then
  sudo rsync -aAHX  --delete /opt/homebrew/opt/socket_vmnet/ /opt/socket_vmnet/
fi
