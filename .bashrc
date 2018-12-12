#!/usr/bin/env bash
#
# ~/.bashrc
#

# Depends on my ~/.profile

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#-------------------------#
# Bash configuration
#-------------------------#
shopt -s histappend autocd globstar histreedit histverify

export HISTTIMEFORMAT="%F %T "
export HISTCONTROL=ignorespace:ignoredups:erasedups
export HISTSIZE=
export HISTFILESIZE=
export HISTIGNORE="ls:la:l:ll:lla:lls:sl:gst:glg:g diff:..::...:s:mtog:cd:cd -:clear:reb:exit"
PS1='[\u@\h \W]\$ '

if [[ $(uname -a) =~ Darwin ]]; then
    export MACOS=true
fi

# Enabled blocked forward incrmental search
# http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=383760
stty -ixon

#-------------------------#
# Aliases
#-------------------------#
alias df='df -h'
alias du='du -hs'
alias free='free -m'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'


if [[ $MACOS ]]; then
    alias ls='ls -G'
else
    alias ls='ls --color=auto'
fi

alias reb='exec bash'
alias aria2c='aria2c -j2 --seed-time 0 --max-upload-limit 1'

git-branch () {
    git branch | grep "\*" | cut -d " " -f 2
}

#-------------------------#
# Prompt
#-------------------------#

red="\[$(tput setaf 1)\]"
green="\[$(tput setaf 2)\]"
yellow="\[$(tput setaf 3)\]"
blue="\[$(tput setaf 4)\]"
cyan="\[$(tput setaf 6)\]"
bold="\[$(tput bold)\]"
plain="\[$(tput sgr0)\]"

prompt() {
    EXIT_STATUS="$?"

    if git branch >/dev/null 2>/dev/null; then
        # This is a git repo
        VC_CHAR='±'
        VC_REF=$(
            git symbolic-ref --short HEAD 2> /dev/null || {
                git rev-list --max-count=1 1 HEAD 2>/dev/null | cut -c 1-8
            }
        )
        if [[ -z "$(git status --porcelain)" ]]; then
            VC_STATUS=""
        else
            VC_STATUS="*"
        fi
        VC_INFO="$bold$green $VC_CHAR $plain$cyan($VC_REF$VC_STATUS)$plain"
    else
        VC_INFO=""
    fi

    if [[ "$USER" == "root" ]]; then
        _PSYMBOL="#"
    else
        _PSYMBOL=">"
    fi

    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
        _HNCOLOUR=$cyan$bold
    else
        _HNCOLOUR=$cyan
    fi

    if [[ $EXIT_STATUS != "0" ]]; then
        _PUSER_COLOR=$red
    else
        _PUSER_COLOR=$green
    fi

    if [[ -n "$VIRTUAL_ENV" ]];then
        _PVENV=" $blue($(basename "$VIRTUAL_ENV"))"
    else
        _PVENV=""
    fi

    _PUSER="$_PUSER_COLOR$bold\u$green$bold$plain@$_HNCOLOUR\h$plain$blue"
    _PTIME="$plain$yellow\@$blue"
    _PDIR="$plain$green\W$blue"

    _L1="┌─[$_PUSER]-[$_PTIME]$VC_INFO$_PVENV"
    _L2="└─($_PDIR)-$_PSYMBOL"

    PS1="$blue$_L1\n$blue$_L2 $plain"
}

PROMPT_DIRTRIM=2
PROMPT_COMMAND=prompt

preexec () {
    history -a

    if [[ "$EMACS" ]]; then
        return;
    fi
}

# Inspired by http://superuser.com/a/175802/57412
preexec_invoke_exec () {
    [ -n "$COMP_LINE" ] && return  # do nothing if completing

    if [[ $PROMPT_COMMAND == $BASH_COMMAND* ]]; then
        # The current command is one the prompt command i.e. a prompt
        # execution.
        preexec "$BASH_COMMAND"
        return
    fi
}
trap 'preexec_invoke_exec' DEBUG

#-------------------------#
# Functions
#-------------------------#
function set-nvim-listen-address {
    local server_name="default"
    local tmux_window=$(tmux display-message -p '#S-#I' 2> /dev/null)
    local i3_ws=$(i3-msg -t  get_workspaces 2> /dev/null| \
        python -mjson.tool 2> /dev/null | \
        grep -E '"focused":\s*true' -A 1 | \
        grep "name" | \
        cut -d '"' -f 4 2> /dev/null)

    if [ "$tmux_window" ]; then
        server_name="tmux-$tmux_window"
    elif [ "$i3_ws" ]; then
        server_name="i3-$i3_ws"
    fi

    # Set the NVIM listen address. Works for the server (launched via `nvim`)
    # and client (launched via `nvr`).
    export NVIM_LISTEN_ADDRESS=/tmp/${ws:-$1}-vim.sock
}

function reload-shell () {
    source ~/.profile
    exec $SHELL
}
#-------------------------#
# Initializations
#-------------------------#
set-nvim-listen-address

[[ -s "/usr/local/etc/profile.d/autojump.sh" ]] && source "/usr/local/etc/profile.d/autojump.sh"
[[ -s "/usr/share/autojump/autojump.sh" ]] && source "/usr/share/autojump/autojump.sh"
[[ -s "/etc/profile.d/autojump.bash" ]] && source "/etc/profile.d/autojump.bash"

# Initialize fzf
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

if [ -e "$HOME/.bashrc.local" ]; then
    source "$HOME/.bashrc.local"
fi

# Exit with success
true

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
