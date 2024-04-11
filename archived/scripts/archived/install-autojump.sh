#!/usr/bin/env bash

RELEASE_URL="https://api.github.com/repos/xen0n/autojump-rs/releases/latest"
DOWNLOAD_PREFIX="https://github.com/xen0n/autojump-rs/releases/download"
LOCAL_BIN="$HOME/.local/bin"
LOCAL_PROFILE_DIR=$HOME/.config/profile.d/

download() {
    mkdir -p $LOCAL_BIN
    DOWNLOAD_LINK=$(curl -s "$RELEASE_URL" | grep -Eo "$DOWNLOAD_PREFIX.+$1.tar.gz")
    curl -sL $DOWNLOAD_LINK | tar -xzC $LOCAL_BIN
}

if [ ! -x $HOME/.cargo/bin/autojump ]; then
    archi=$(uname -sm)
    case "$archi" in
        Darwin\ x86_64) download x86_64-apple-darwin;;
        Linux\ x86_64)  download x86_64-unknown-linux-musl;;
        Linux\ armv7l)  download armv7-unknown-linux-gnueabihf;;
        Linux\ aarch64) download aarch64-unknown-linux-gnu;;
        *) ;;
    esac
fi

mkdir -p $LOCAL_PROFILE_DIR
cd $LOCAL_PROFILE_DIR
curl -s --remote-name-all \
    https://raw.githubusercontent.com/wting/autojump/master/bin/autojump.{zsh,bash}
