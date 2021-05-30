#!/usr/bin/env bash
#
# ~/.bashrc
#

# Depends on my ~/.profile

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

if [ "$CONFIGS_SRC_DIR" ]; then
    source $CONFIGS_SRC_DIR/.shell_functions
fi

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

alias re='exec bash'
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
        _HNCOLOUR=$red
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
# Initializations
#-------------------------#
[[ -s "$HOME/.config/profile.d/autojump.bash" ]] && source "$HOME/.config/profile.d/autojump.bash"

# Initialize fzf
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
[ -f ~/.skim/shell/key-bindings.bash ] && source ~/.skim/shell/key-bindings.bash

if [ -e "$HOME/.bashrc.local" ]; then
    source "$HOME/.bashrc.local"
fi

# Exit with success
true
