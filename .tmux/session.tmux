# vim: filetype=tmux
send-keys nvim\n
split-window
split-window
select-layout -t 0 main-vertical

new-window -t 9
send-keys btm\n
split-window
split-window
select-layout -t 9 main-vertical
select-window -t 0
