#!/usr/bin/env bash

ARCH=$(uname -sm)

case $ARCH in
    "Linux x86_64")
      ARCHIVE=nvim-linux-x86_64
      ;;
    "Linux aarch64")
      ARCHIVE=nvim-linux-arm64
      ;;
esac

INSTALLED_VERSION=$(nvim --version | grep -Eo 'v[[:digit:].]+$')
LATEST_VERSION=$(curl -s https://api.github.com/repos/neovim/neovim/releases/tags/nightly | grep tag_name | grep -Eo 'v[[:digit:].]+')

if [ -n "$ARCHIVE" -a "$INSTALLED_VERSION" != "$LATEST_VERSION" ]; then
  TARBALL_URL="https://github.com/neovim/neovim/releases/download/nightly/$ARCHIVE.tar.gz"
  echo "Installing 'neovim' $LATEST_VERSION..."
  mkdir -p $HOME/.local/bin
  curl -sL $TARBALL_URL  | tar -xz -C $HOME/.local/
  ln -sf $HOME/.local/$ARCHIVE/bin/nvim $HOME/.local/bin/nvim
  echo "..done!"
fi
