#!/usr/bin/env bash
set -e

HOME=/var/root sudo nix-collect-garbage -d
nix-collect-garbage -d
brew cleanup --prune=all
uv clean
if [ -n "$(command -v mise)" ]; then
  mise prune -y
fi
