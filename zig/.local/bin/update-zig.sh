#!/usr/bin/env bash

# Put this somewhere in the path and run update-zig.sh to get the latest master
# build for zig.
#
# Ensure that `$HOME/.local/zig` is in path. For fish:
# fish_add_path ~/.local/zig

set -e

DOWNLOAD_DIR=/tmp/zig
ZIG_DIR=$HOME/.local/zig

case "$(uname -sm)" in
    "Darwin arm64") ARCH="aarch64-macos";;
    "Linux x86_64")  ARCH="x86_64-linux";;
    "Linux aarch64") ARCH="aarch64-linux";;
    *) ;;
esac

RELEASE_INFO=$(curl -s "https://ziglang.org/download/index.json" | jq ".master[\"$ARCH\"]")
ARCHIVE=$(echo $RELEASE_INFO | jq .tarball -r)
RELEASE_NAME=$(basename $ARCHIVE .tar.xz)

mkdir -p $DOWNLOAD_DIR $(dirname ZIG_DIR)

curl $ARCHIVE | tar -C $DOWNLOAD_DIR -Jxf -

rm -rf $ZIG_DIR
mv $DOWNLOAD_DIR/$RELEASE_NAME $ZIG_DIR

if [ -z "$(command -v zig)" ]; then
  echo "Ensure that $ZIG_DIR is added to your path!"
fi

rm -rf $DOWNLOAD_DIR
