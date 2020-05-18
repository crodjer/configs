#!/usr/bin/env bash

function run_command {
    local cmd=$1
    local log=$2

    echo "[$log] Running: $cmd"
    bash -c "$cmd" &> /tmp/cron-$log.log
}

# run_command "docker container prune -f && docker image prune -af" docker-cleanup
run_command "docker container prune -f"  docker-container-cleanup
run_command "$HOME/.cargo/bin/rustup update"  rustup-update
run_command "$HOME/.cargo/bin/cargo +nightly install-update racer"  racer-update
run_command "$HOME/.cargo/bin/cargo install-update -a"  cargo-update
run_command "cd $HOME/projects/alacritty && git pull && cargo build --release"  alacritty-update
run_command "cd $HOME/projects/tmux && git pull && ./autogen.sh && make"  tmux-update
