#!/usr/bin/env bash

# Runs [gemini-cli](https://github.com/google-gemini/gemini-cli) in a firejail.
#
# Downloads a pre-built `gemini.js` from github releases and then uses
# `firejail` and `deno` permissioning system to run the cli in a restricted
# environment.
#
# There's still potential for damage, but just a bit limited.

GEMINI_HOME=$HOME/.gemini
GEMINI_JS=$GEMINI_HOME/gemini.js
TMP_GEMINI_JS=/tmp/gemini.js

RELEASE_PAGE="https://api.github.com/repos/google-gemini/gemini-cli/releases/latest"
DOWNLOAD_URL="https://github.com/google-gemini/gemini-cli/releases/latest/download/gemini.js"
VERSION="none"

if [ -f $GEMINI_JS ]; then
  VERSION=$(grep "CLI_VERSION =" ~/.gemini/gemini.js | grep -Po "[\d.]+")
  CURRENT_VERSION=$(curl -s $RELEASE_PAGE | grep tag_name | grep -Po "[\d.]+")

fi

if [ $VERSION != $CURRENT_VERSION ]; then
  echo $VERSION, $CURRENT_VERSION
  curl -fsL $DOWNLOAD_URL -o $TMP_GEMINI_JS && mv $TMP_GEMINI_JS $GEMINI_JS
fi

DENO=$(which deno)
DENO_CACHE=$HOME/.cache/deno
MISE_DIR=$HOME/.local/share/mise

ACCESSIBLE_DIRS="$PWD,$GEMINI_HOME,$DENO_CACHE,$MISE_DIR"

# Configure more complex firejail profile in `~/.config/firejail/deno.profile`
DENO_COMPAT=1 firejail \
  --whitelist=$PWD \
  --whitelist=$MISE_DIR \
  --whitelist=$DENO_CACHE \
  --whitelist=$GEMINI_HOME \
  $DENO \
  --allow-sys=osRelease,homedir,uid --allow-read \
  --deny-env="*KEY,*SECRET" --allow-env \
  --allow-run \
  --allow-write=$ACCESSIBLE_DIRS \
  $GEMINI_JS
