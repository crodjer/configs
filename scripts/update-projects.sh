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
  run_command "cargo install-update -a"  cargo-update
fi

if [ -d $HOME/projects/forks/gnvim/ ]; then
  run_command "cd $HOME/projects/forks/gnvim && git pull && make && PREFIX=~/.local make install"  neovim-update
fi

if [ -d $HOME/projects/forks/yofi/ ]; then
  run_command "cd $HOME/projects/forks/yofi && git pull && cargo build --release"  yofi-update
fi

if [ -x "$(command -v autojump)" ]; then
  run_command "$SCRIPTS_DIR/install-autojump.sh" autojump-update
fi
