#!/usr/bin/env bash

# Runs [gemini-cli](https://github.com/google-gemini/gemini-cli) in bwrap.
#
# Downloads a pre-built `gemini.js` from github releases and then uses
# `bwrap` and `deno` permissioning system to run the cli in a restricted
# environment.
#
# There's still potential for damage, but just a bit limited.

GEMINI_HOME=$HOME/.gemini
GEMINI_CLI_DIR=$GEMINI_HOME/cli
TMP_GEMINI_ZIP=/tmp/gemini.zip

RELEASE_PAGE="https://api.github.com/repos/google-gemini/gemini-cli/releases/latest"
DOWNLOAD_URL="https://github.com/google-gemini/gemini-cli/releases/latest/download/gemini-cli-bundle.zip"

VERSION="none"

gemini-wrapped () {
  bwrap \
    --unshare-user --unshare-ipc --unshare-pid --unshare-uts \
    --unshare-cgroup \
    --ro-bind /usr /usr \
    --symlink usr/lib /lib \
    --symlink usr/lib64 /lib64 \
    --symlink usr/bin /bin \
    --symlink usr/sbin /sbin \
    --dev /dev \
    --proc /proc \
    --tmpfs /tmp \
    --tmpfs /run \
    --ro-bind /etc/passwd /etc/passwd \
    --ro-bind /etc/group /etc/group \
    --ro-bind /etc/hosts /etc/hosts \
    --ro-bind /etc/nsswitch.conf /etc/nsswitch.conf \
    --ro-bind /etc/resolv.conf /etc/resolv.conf \
    --ro-bind /etc/ssl /etc/ssl \
    --ro-bind /etc/localtime /etc/localtime \
    --bind "${HOME}/.local/share/mise" "${HOME}/.local/share/mise" \
    --bind "${HOME}/.local/state/mise" "${HOME}/.local/state/mise" \
    --bind "$HOME/.npm" "$HOME/.npm" \
    --bind "$GEMINI_HOME" "$GEMINI_HOME" \
    --bind "$PWD" "$PWD" \
    --chdir "$PWD" \
    -- node $GEMINI_CLI_DIR/gemini.js $@
}

if [ -d "$GEMINI_CLI_DIR" ]; then
  VERSION=$(gemini-wrapped --version)
  CURRENT_VERSION=$(curl -s $RELEASE_PAGE | grep tag_name | grep -Po "[\d.]+")
fi

if [ "$VERSION" != "$CURRENT_VERSION" ]; then
  curl -fsL $DOWNLOAD_URL -o $TMP_GEMINI_ZIP
  unzip -ud $GEMINI_CLI_DIR /tmp/gemini.zip
fi

gemini-wrapped $@
