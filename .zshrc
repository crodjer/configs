#!/usr/bin/env zsh
#-------------------------#
# ~/.zshrc
#
# Depends on my ~/.profile

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#-------------------------#
# Options
#-------------------------#
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=100000
setopt autocd correct extended_glob hist_ignore_all_dups \
    hist_expire_dups_first hist_ignore_space notify prompt_subst share_history

unsetopt beep correct_all
bindkey -e
autoload -U select-word-style
select-word-style bash

if [[ $(uname -a) =~ Darwin ]]; then
    local MACOS=true
fi

#-------------------------#
# Completion
#-------------------------#
zstyle ':completion:*' completer _expand _complete _ignored
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}'
zstyle ':completion:*' menu select
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename "$HOME/.zshrc"
bindkey '^[n' expand-or-complete
bindkey '^[p' reverse-menu-complete
bindkey '^[[Z' reverse-menu-complete

autoload -Uz compinit
compinit

#-------------------------#
# Aliases
#-------------------------#
alias rez='exec zsh'

if [ -n "$MACOS" ]; then
    alias ls='ls -G'
else
    alias ls='ls --color=auto'
fi

#-------------------------#
# Utility functions
#-------------------------#
gen-prompt () {
    local prev_exit="$?"
    local user_color="green"

    if [ $prev_exit -eq 1 ]; then
        user_color="red"
    fi

    ## Generate a simple, informative multiline prompt.
    local _line_1="%F{blue}┌─[%B%F{$user_color}%n%f%b@%F{green}%m%f%F{blue}]-[%F{yellow}%*%f%F{blue}]%f"
    local _line_2="%F{blue}└─%(!.#.>)%f "

    # Render git info, if available, in the prompt.
    if git branch >/dev/null 2>/dev/null; then

        local _git_branch=$(git symbolic-ref --short HEAD 2> /dev/null || {
            git rev-list --max-count=1 HEAD 2>/dev/null | cut -c 1-8
        })
        local _git_status="$_git_branch$([ "$(git status --porcelain)" ] && echo "*")"
        _line_1="$_line_1 %B%F{green}±%b %F{cyan}($_git_status)%f"
    fi

    # Render python virtual env info, if available, in the prompt.
    if [[ -n "$VIRTUAL_ENV" ]];then
        local _venv_prompt=""
        _line_1="$_line_1 %F{blue}($(basename "$VIRTUAL_ENV"))%f"
    fi

    local _newline=$'\n'

    echo "$_line_1$_newline$_line_2"
}

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
    export NVIM_LISTEN_ADDRESS=/tmp/nvim-${server_name:-$1}-vim.sock
}

function reload-shell () {
    source ~/.profile
    exec $SHELL
}


#-------------------------#
# Initializations
#-------------------------#
PROMPT='$(gen-prompt)'
RPROMPT="%F{green}%~%f"

# Initialize autojump
[[ -s "/usr/local/etc/profile.d/autojump.sh" ]] && source "/usr/local/etc/profile.d/autojump.sh"
[[ -s "/usr/share/autojump/autojump.sh" ]] && source "/usr/share/autojump/autojump.sh"
[[ -s "/etc/profile.d/autojump.zsh" ]] && source "/etc/profile.d/autojump.zsh"

# Set the default Vim server name.
set-nvim-listen-address

# Initialize fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Load local bashrc configs as well.
if [ -e "$HOME/.bashrc.local" ]; then
    source "$HOME/.bashrc.local"
fi

if [ -e "$HOME/.zshrc.local" ]; then
    source "$HOME/.zshrc.local"
fi

# Exit with success
true
