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

if [[ $(uname -a) =~ Darwin ]]; then
    local MACOS=true
fi

debug_shell () {
    if [ "$DEBUG_SHELL" ]; then
        if [[ "$MACOS"  ]]; then
            echo "$(gdate +'%T-%N') $@"
        else
            echo "$(date +'%T-%N') $@"
        fi
    fi
}

debug_shell ZSH: Options
#-------------------------#
# Options
#-------------------------#
HISTFILE=~/.zsh_history
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

fpath+=$HOME/.config/zsh/functions
if type brew &>/dev/null
then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi
autoload -Uz compinit
compinit -u

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

if [ -x "$(command -v fdfind)" ]; then
    alias fd="fdfind"
fi

if [ -n "$MACOS" ]; then
    alias ls='ls -G'
else
    alias ls='ls --color=auto'
fi

alias exa='exa --icons'

alias cb='xclip -selection clipboard'
alias k='kubectl'
if [ -x "$(command -v iwctl)" ]; then
    alias wifi="iwctl station $(iwctl device list | grep station | grep -Eo '\w+' | head -1)"
fi

debug_shell ZSH: Utility functions

#-------------------------#
# Utility functions
#-------------------------#
git_branch () {
    local git_branch=$(git symbolic-ref --short HEAD 2> /dev/null || {
        git rev-list --max-count=1 HEAD 2>/dev/null | cut -c 1-8
    })

    echo $git_branch
}

function set-nvim-listen-address {
    local server_name="default"
    if [ "$TMUX" ]; then
        local tmux_window=$(tmux display-message -p '#S-#I' 2> /dev/null)
        server_name="tmux-$tmux_window"
        return
    fi
    local sway_ws=$(swaymsg -t  get_workspaces 2> /dev/null | \
       jq -r "map(select(.focused == true)) | map(.name) | first")
    local i3_ws=$(i3-msg -t  get_workspaces 2> /dev/null | \
       jq -r "map(select(.focused == true)) | map(.name) | first")

    if [ "$sway_ws" ]; then
        server_name="sway-$sway_ws"
    elif [ "$i3_ws" ]; then
        server_name="i3-$i3_ws"
    fi

    # Set the NVIM listen address. Works for the server (launched via `nvim`)
    # and client (launched via `nvr`).
    export NVIM_LISTEN_ADDRESS="/tmp/nvim-${server_name:-$1}.sock"
}
autoload -U add-zsh-hook
add-zsh-hook chpwd set-nvim-listen-address
set-nvim-listen-address

gen-prompt () {
    local prev_exit="$?"
    local user_color="green"

    if [ $prev_exit -eq 1 ]; then
        user_color="red"
    fi

    local host_color="gray"

    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
        host_color="red"
    fi

    local prompt_alias=~$USER/.prompt-user
    ## Generate a simple, informative multiline prompt.
    if [ -f $prompt_alias ]; then
        local user_name=$(cat $prompt_alias)
    else
        local user_name="$USER"
    fi

    local _line_1="%F{blue}┌─[%B%F{$user_color}${user_name}%f%b@%F{${host_color}}%m%f%F{blue}]-[%F{yellow}%*%f%F{blue}]%f"
    local _line_2="%F{blue}└─%(!.#.>)%f "

    git_branch=$(git_branch)

    # Render git info, if available, in the prompt.
    if [ "$git_branch" ]; then
        local _git_status="$git_branch$([ "$(git status --porcelain -uno)" ] && echo "*")"
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
elif [[ -e /usr/share/zsh/manjaro-zsh-prompt ]]; then
  USE_POWERLINE="true"
  source /usr/share/zsh/manjaro-zsh-prompt
  [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
elif [[ -f ~/.p10k.zsh ]]; then
    source ~/.p10k.zsh
else
    PROMPT='$(gen-prompt)'
    RPROMPT="%F{green}%~%f"
fi

debug_shell ZSH: Initializations
#-------------------------#
# Initializations
#-------------------------#

# Initialize autojump
[[ -s "$HOME/.config/profile.d/autojump.zsh" ]] && source "$HOME/.config/profile.d/autojump.zsh"
test -n "$(command -v zoxide)" && eval "$(zoxide init zsh)"

# Initialize zbell
ZLONG_ALERT_FILE="$HOME/.config/zsh/zlong_alert.zsh"
if [ ! -f "$ZLONG_ALERT_FILE" ]; then
    curl -sfLo $ZLONG_ALERT_FILE --create-dirs "https://raw.githubusercontent.com/kevinywlui/zlong_alert.zsh/master/zlong_alert.zsh"
fi
source $ZLONG_ALERT_FILE
# Initialize mise
if [ -n "$(command -v mise)" ]; then
    eval "$(mise activate zsh)"
fi

if [ -n "$(command -v rtx)" ]; then
    eval "$(rtx activate zsh)"
fi

# Initialize fzf
[ -f /usr/share/doc/fzf/examples/key-bindings.zsh ] && source /usr/share/doc/fzf/examples/key-bindings.zsh
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f ~/.skim/shell/key-bindings.zsh ] && source ~/.skim/shell/key-bindings.zsh

# Get the correct GPG TTY
export GPG_TTY=$(tty)

# Load local bashrc configs as well.
if [ -e "$HOME/.bashrc.local" ]; then
    source "$HOME/.bashrc.local"
fi

if [ -e "$HOME/.zshrc.local" ]; then
    source "$HOME/.zshrc.local"
fi

# PATH
export PATH=$PATH:$HOME/.local/bin

# Direnv
test -n "$(command -v direnv)" && eval "$(direnv hook zsh)"

# Rust
if [ -e "$HOME/.cargo/env" ]; then
    source "$HOME/.cargo/env"
fi

# Go
export GOPATH=~/.local/share/go

debug_shell ZSH: Done

# Exit with success
true
