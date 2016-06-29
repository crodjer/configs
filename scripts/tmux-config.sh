#!/usr/bin/env bash

dir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
configs_dir="$dir/.."

if [[ $(uname -a) =~ Darwin ]]; then
    # As usual, we need to handle OSX quirks.
    session=$(hostname | cut -d '.' -f 1)
else
    session=$(hostname)
fi

if [ $(command -v htop) ]; then
    # System may have issues with top, hence htop is available. Use that.
    top_command=htop
else
    top_command=top
fi

attach-windows () {
    sleep 0.1
    tmux split-window -hc $configs_dir # 0 (split)
    tmux new-window                    # 1
    tmux new-window -t 9 $top_command  # 9
    tmux split-window -t 9 -h          # 9 (secondary monitoring)
    tmux split-window -t 9 -v          # 9 (tertiary monitoring)
    tmux select-window -t :0
    tmux select-pane -t 0
}

tmux attach-session -t $session || {
    attach-windows &
    tmux new-session -s $session
}
