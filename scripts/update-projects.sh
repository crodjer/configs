#!/usr/bin/env bash

function run_command {
  local cmd=$1
  local log=$2

  echo "[$log] Running: $cmd"
  bash -c "$cmd" &> /tmp/update-projects-$log.log
}

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

if [ -d $HOME/projects/alacritty/ ]; then
  run_command "cd $HOME/projects/alacritty && git pull && cargo build --release"  alacritty-update
fi

if [ -d $HOME/projects/tmux/ ]; then
  run_command "cd $HOME/projects/tmux && git pull && ./autogen.sh && make"  tmux-update
fi

if [ -x "$(command -v autojump)" ]; then
  LOCAL_PROFILE_DIR=$HOME/.config/profile.d/
  mkdir -p $LOCAL_PROFILE_DIR
  run_command "cd $LOCAL_PROFILE_DIR && wget -c https://raw.githubusercontent.com/wting/autojump/master/bin/autojump.{zsh,bash}" autojump-scripts-update
fi
