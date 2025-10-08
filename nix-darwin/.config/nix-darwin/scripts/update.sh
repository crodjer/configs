#!/usr/bin/env bash

set -e

HOME=/var/root sudo nix-channel --update
HOME=/var/root sudo darwin-rebuild switch
brew upgrade
pipx upgrade-all
cargo install-update --all
