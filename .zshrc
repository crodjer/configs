#!/usr/bin/env zsh
#-------------------------#
# ~/.zshrc
#
# Depends on my ~/.profile

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

if [ "$CONFIGS_SRC_DIR" ]; then
    source $CONFIGS_SRC_DIR/.shell_functions
fi

debug_shell () {
    if [ "$DEBUG_SHELL" ]; then
        echo "$(date +'%T-%N') $@"
    fi
}

debug_shell ZSH: Options
#-------------------------#
# Options
#-------------------------#
HISTFILE=~/.histfile
HISTSIZE=50000
SAVEHIST=100000
REPORTTIME=3
setopt hist_ignore_all_dups hist_ignore_space share_history extended_history
setopt hist_expire_dups_first
setopt autocd correct extended_glob notify prompt_subst

unsetopt beep correct_all
bindkey -e
autoload -U select-word-style
select-word-style bash

if [[ $(uname -a) =~ Darwin ]]; then
    local MACOS=true
fi

debug_shell ZSH: Completion

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

fpath+=~/.zfunc

autoload -Uz compinit
# Instead of having compinit happen on every prompt, use the cached version in
# the prompt and have it re-load every 5 minutes via Cron
# * * * * * zsh -i -c 'compinit'
if [[ "$MACOS" && $(date +'%j') != $(stat -f '%Sm' -t '%j' ~/.zcompdump) ]]; then
	compinit;
elif [[ -n $HOME/.zcompdump(#qN.mh+1) ]]; then
	compinit;
else
	compinit -C;
fi;

debug_shell ZSH: Aliases

#-------------------------#
# Aliases
#-------------------------#
alias re='exec zsh'

if [ -n "$MACOS" ]; then
    alias ls='ls -G'
else
    alias ls='ls --color=auto'
fi

alias cb='xclip -selection clipboard'
alias k='kubectl'

debug_shell ZSH: Utility functions

#-------------------------#
# Utility functions
#-------------------------#
gen-prompt () {
    local prev_exit="$?"
    local user_color="green"

    if [ $prev_exit -eq 1 ]; then
        user_color="red"
    fi

    local host_color="green"

    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
        host_color="red"
    fi

    ## Generate a simple, informative multiline prompt.
    local _line_1="%F{blue}┌─[%B%F{$user_color}%n%f%b@%F{${host_color}}%m%f%F{blue}]-[%F{yellow}%*%f%F{blue}]%f"
    local _line_2="%F{blue}└─%(!.#.>)%f "

    local git_branch=$(git symbolic-ref --short HEAD 2> /dev/null || {
        git rev-list --max-count=1 HEAD 2>/dev/null | cut -c 1-8
    })

    # Render git info, if available, in the prompt.
    if [ "$git_branch" ]; then
        local _git_status="$git_branch$([ "$(git status --porcelain)" ] && echo "*")"
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

debug_shell ZSH: Prompt
#-------------------------#
# Prompt
#-------------------------#
if [ -x "$(command -v starship)" ] && \
    [ -f $HOME/.config/starship.toml ]; then
    eval "$(starship init zsh)"
else
    PROMPT='$(gen-prompt)'
    RPROMPT="%F{green}%~%f"
fi

debug_shell ZSH: Initializations
#-------------------------#
# Initializations
#-------------------------#

# Initialize autojump
[[ -s "/usr/local/etc/profile.d/autojump.sh" ]] && source "/usr/local/etc/profile.d/autojump.sh"
[[ -s "/usr/share/autojump/autojump.sh" ]] && source "/usr/share/autojump/autojump.sh"
[[ -s "/etc/profile.d/autojump.zsh" ]] && source "/etc/profile.d/autojump.zsh"
[[ -s "$HOME/.config/profile.d/autojump.zsh" ]] && source "$HOME/.config/profile.d/autojump.zsh"

# Initialize fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f ~/.skim/shell/key-bindings.zsh ] && source ~/.skim/shell/key-bindings.zsh

# Load local bashrc configs as well.
if [ -e "$HOME/.bashrc.local" ]; then
    source "$HOME/.bashrc.local"
fi

if [ -e "$HOME/.zshrc.local" ]; then
    source "$HOME/.zshrc.local"
fi

debug_shell ZSH: Done

# Exit with success
true
