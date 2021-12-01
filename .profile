#!/bin/sh
#
# POSIX Shell login script.
#
#   Rohan Jain
#       ~crodjer
#
# Borrowed from: http://stuff.lhunath.com/.profile

# function debug_shell () {
#     if [ "$DEBUG_SHELL" ]; then
#         echo "$(date +'%T-%N') $@"
#     fi
# }

export PROFILE_SRC=$(readlink "$(readlink ~/.profile)" || readlink ~/.profile)
export CONFIGS_SRC_DIR=$(dirname $PROFILE_SRC)

#-------------------------#
# BASE - PATH             #
#-------------------------#
# debug_shell Profile: Path
ICED_PATH="$HOME/.config/nvim/plugged/vim-iced/bin"

if [ -z "$_ORIGINAL_PATH" ]; then
    export _ORIGINAL_PATH=$PATH
else
    # We seem to be reloading .profile
    PATH=$_ORIGINAL_PATH
fi

export                        PATH="$PATH:$HOME/.bin"
                              PATH="$PATH:$HOME/.local/bin:$HOME/.local/sbin"
                              PATH="$PATH:$CONFIGS_SRC_DIR/scripts"
                              PATH="$PATH:$HOME/.cargo/bin"
[ -d "$ICED_PATH" ]        && PATH="$PATH:$ICED_PATH"

if [ -z "$_ORIGINAL_MANPATH" ]; then
    export _ORIGINAL_MANPATH=$MANPATH
else
    # We seem to be reloading .profile
    MANPATH=$_ORIGINAL_MANPATH
fi
export                      MANPATH="$MANPATH:$HOME/.man:/usr/local/share/man:/usr/local/man"
[ -d "$HOME/.local" ]   &&  MANPATH="$MANPATH:$HOME/.local/share/man:$HOME/.local/man"

if [ -z "$_ORIGINAL_XDG_DATA_DIRS" ]; then
    export _ORIGINAL_XDG_DATA_DIRS=$XDG_DATA_DIRS
else
    # We seem to be reloading .profile
    XDG_DATA_DIRS=$_ORIGINAL_XDG_DATA_DIRS
fi

if [ -d "/var/lib/flatpak/exports/share/applications" ]; then
    if [ -z "$XDG_DATA_DIRS" ]; then
        XDG_DATA_DIRS=/usr/share:/usr/share:/usr/local/share
    fi
    export XDG_DATA_DIRS=/home/$USER/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share:$XDG_DATA_DIRS
fi

#-------------------------#
# BASE - Environment
#-------------------------#
# debug_shell Profile: Environment
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
# debug_shell Profile: Applications
export LESS=" -R "
command -v source-highlight-esc.sh > /dev/null && \
    LESSOPEN="| $(which source-highlight-esc.sh) %s 2> /dev/null"
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
# debug_shell Profile: Local
# shellcheck source=/dev/null
[ -r "$HOME/.profile.local" ] && . "$HOME/.profile.local"

# debug_shell Profile: Done
true # Exit status should be 0.
