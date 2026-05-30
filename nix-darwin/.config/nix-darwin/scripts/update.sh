#!/usr/bin/env bash

set -e

HOME=/var/root sudo nix-channel --update
HOME=/var/root sudo darwin-rebuild switch
brew upgrade
cargo install-update --all
uv tool upgrade --all

if [ -n "$(command -v pnpm)" ]; then
  pnpm -g upgrade
fi

if [ -n "$(command -v mise)" ]; then
  mise upgrade
fi

if [ -d /opt/homebrew/opt/socket_vmnet/ ]; then
  sudo rsync -aAHX  --delete /opt/homebrew/opt/socket_vmnet/ /opt/socket_vmnet/
fi
