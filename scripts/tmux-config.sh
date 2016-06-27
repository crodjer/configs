#!/usr/bin/env bash

dir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
configs_dir="$dir/.."

if [[ $(uname -a) =~ Darwin ]]; then
    # As usual, we need to handle a
    session=$(hostname | cut -d '.' -f 1)
    # Top in OSX
    top_command=htop
else
    session=$(hostname)
    top_command=top
fi

attach-windows () {
    sleep 0.1
    send-keys                          # 0
    tmux split-window -hc $configs_dir # 0 (split)
    tmux new-window                    # 1
    tmux new-window                    # 2
    tmux new-window                    # 3
    tmux new-window                    # 4
    tmux new-window                    # 5
    tmux new-window                    # 6
    tmux new-window                    # 7
    tmux new-window                    # 8
    tmux new-window $top_command       # 9
    tmux split-window -h               # 9 (secondary monitoring)
    tmux split-window -v               # 9 (tertiary monitoring)

    select-window -t :0
}

tmux attach-session -t $session || {
    attach-windows &
    tmux new-session -s $session
}
