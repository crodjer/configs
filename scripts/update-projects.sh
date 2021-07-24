#!/usr/bin/env bash

function run_command {
  local cmd=$1
  local log=$2

  echo "[$log] Running: $cmd"
  bash -c "$cmd" &> /tmp/update-projects-$log.log
}

SCRIPTS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

if [ -x "$(command -v docker)" ]; then
  run_command "docker container prune -f && docker image prune -af" docker-cleanup
fi

if [ -x "$(command -v rustup)" ]; then
  run_command "rustup update"  rustup-update
fi

if [ -x "$(command -v cargo)" ]; then
  # run_command "cargo +nightly install-update racer"  racer-update
  run_command "cargo install-update -a"  cargo-update
fi

# if [ -d $HOME/projects/gnvim/ ]; then
#   run_command "cd $HOME/projects/gnvim && git pull && make"  neovim-update
# fi

if [ -x "$(command -v autojump)" ]; then
  run_command "$SCRIPTS_DIR/install-autojump.sh" autojump-update
fi
