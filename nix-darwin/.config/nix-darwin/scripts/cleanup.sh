#!/usr/bin/env bash
set -e

HOME=/var/root sudo bash -c 'nix-collect-garbage -d'
nix-collect-garbage -d
brew cleanup --prune=all
