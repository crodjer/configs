#!/usr/bin/env bash
set -e

HOME=/var/root sudo nix-collect-garbage -d
nix-collect-garbage -d
brew cleanup --prune=all
