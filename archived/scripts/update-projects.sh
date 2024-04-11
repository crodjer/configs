#!/usr/bin/env bash

function run_command {
  local cmd=$1
  local log=$2

  echo "[$log] Running: $cmd"
  bash -c "$cmd" &> /tmp/update-projects-$log.log
}

SCRIPTS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

if [ -x "$(command -v rustup)" ]; then
  run_command "rustup update"  rustup-update
fi

if [ -x "$(command -v cargo)" ]; then
  run_command "cargo install-update -a"  cargo-update
fi

if [ -x "$(command -v pipx)" ]; then
  run_command "pipx upgrade-all" pipx-update
fi
