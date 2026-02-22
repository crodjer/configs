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
GEMINI_WRAPPER=$GEMINI_HOME/wrapper.js

# We dynamically create a tiny ES module wrapper to load the gemini cli.
# Deno's Node compatibility layer sometimes drops or mishandles the standard
# file descriptors (fd) when running inside strict firejail profiles, leading
# to terminal crashes. This wrapper forcibly injects them before loading.
cat << JS > "$GEMINI_WRAPPER"
import process from 'node:process';
process.stdout.fd = 1;
process.stderr.fd = 2;
process.stdin.fd = 0;
await import("file://$GEMINI_JS");
JS

RELEASE_PAGE="https://api.github.com/repos/google-gemini/gemini-cli/releases/latest"
DOWNLOAD_URL="https://github.com/google-gemini/gemini-cli/releases/latest/download/gemini.js"
VERSION="none"

if [ -f "$GEMINI_JS" ]; then
  VERSION=$(grep "CLI_VERSION =" "$GEMINI_JS" | grep -Po "[\d.]+")
  CURRENT_VERSION=$(curl -s $RELEASE_PAGE | grep tag_name | grep -Po "[\d.]+")
fi

if [ "$VERSION" != "$CURRENT_VERSION" ]; then
  curl -fsL $DOWNLOAD_URL -o $TMP_GEMINI_JS && mv $TMP_GEMINI_JS $GEMINI_JS
fi

DENO=$(which deno)
DENO_CACHE=$HOME/.cache/deno
MISE_DIR=$HOME/.local/share/mise

ACCESSIBLE_DIRS="$PWD,$GEMINI_HOME,$DENO_CACHE,$MISE_DIR"

# Configure more complex firejail profile in `~/.config/firejail/deno.profile`
DENO_COMPAT=1 exec firejail \
  --quiet \
  --whitelist="$PWD" \
  --whitelist="$MISE_DIR" \
  --whitelist="$DENO_CACHE" \
  --whitelist="$GEMINI_HOME" \
  "$DENO" run -A \
  "$GEMINI_WRAPPER" "$@"
