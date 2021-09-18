#!/bin/bash
#
# POSIX Shell login script.
#
#   Rohan Jain
#       ~crodjer
#
# Borrowed from: http://stuff.lhunath.com/.profile

function debug_shell () {
    if [ "$DEBUG_SHELL" ]; then
        echo "$(date +'%T-%N') $@"
    fi
}

#-------------------------#
# BASE - PATH             #
#-------------------------#
debug_shell Profile: Path
ICED_PATH="$HOME/.config/nvim/plugged/vim-iced/bin"
export                        PATH="$PATH:$HOME/.bin"
                              PATH="$PATH:$HOME/.local/bin:$HOME/.local/sbin"
                              PATH="$PATH:$HOME/configs/scripts"
                              PATH="$PATH:$HOME/documents/configs/scripts"
                              PATH="$PATH:$HOME/.cargo/bin"
[ -d "$ICED_PATH" ]        && PATH="$PATH:$ICED_PATH"

export                      MANPATH="$MANPATH:$HOME/.man:/usr/local/share/man:/usr/local/man"
[ -d "$HOME/.local" ]   &&  MANPATH="$MANPATH:$HOME/.local/share/man:$HOME/.local/man"

# export LD_LIBRARY_PATH="$HOME/.local/lib/:$LD_LIBRARY_PATH"

export XDG_DATA_DIRS=$HOME/.local/share/flatpak/exports/share:$XDG_DATA_DIRS

#-------------------------#
# BASE - Environment
#-------------------------#
debug_shell Profile: Environment
# [ -e /usr/share/zoneinfo/Asia/Kolkata ] && {
#     export TZ="/usr/share/zoneinfo/Asia/Kolkata"
# }

command -v nvim > /dev/null && EDITOR='nvim' || EDITOR='vi'
export EDITOR

export HOSTALIASES=~/.hosts

if [ "$(uname)" = Darwin ]; then
    export MACOS=true
fi

#-----------------------------#
# ENVIRONMENT - APPLICATIONS  #
#-----------------------------#
debug_shell Profile: Applications
export LESS=" -R "
command -v source-highlight-esc.sh > /dev/null && \
    LESSOPEN="| $(which source-highlight-esc.sh) %s 2> /dev/null"
export PROFILE_SRC=$(readlink "$(readlink ~/.profile)" || readlink ~/.profile)
export CONFIGS_SRC_DIR=$(dirname $PROFILE_SRC)
export LESSOPEN
export GREP_COLOR=31
export PAGER=less
export MANPAGER=$PAGER
export SDCV_HISTSIZE=10000
export SDCV_PAGER=less
export NODE_PATH=$HOME/.local/lib/node_modules:/usr/lib/node_modules
export CHESSDIR=$HOME/.chess/
export AUTOSSH_POLL=60
export SCIKIT_LEARN_DATA=~/.scikit_learn_data
export VIRTUAL_ENV_DISABLE_PROMPT=1
export NPM_PACKAGES="$HOME/.npm-packages"
export MANPATH="$HOME/.npm-packages/share/man:$MANPATH"
export GPODDER_HOME="$HOME/.gpodder"
export BAT_THEME="Solarized (light)"
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse'

command -v pyenv > /dev/null && {
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init --path)"
    eval "$(pyenv init -)"
    if [ -n "$(pyenv commands | grep virtualenv-init)" ]; then
        eval "$(pyenv virtualenv-init -)"
    fi
}

#-----------------------------#
# ENVIRONMENT - LOCAL CONFIG  #
#-----------------------------#
debug_shell Profile: Local
# shellcheck source=/dev/null
[ -r "$HOME/.profile.local" ] && . "$HOME/.profile.local"

debug_shell Profile: Done
true # Exit status should be 0.
