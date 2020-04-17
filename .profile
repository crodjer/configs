#!/bin/sh
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
export                        PATH="$HOME/.bin"
[ -d "$HOME/.local" ]      && PATH="$PATH:$HOME/.local/bin:$HOME/.local/sbin"
[ -d "$HOME/.cabal" ]      && PATH="$PATH:$HOME/.cabal/bin"
[ -d "/opt/local" ]        && PATH="$PATH:/opt/local/bin:/opt/local/sbin"
[ -d "/bin/vendor_perl" ]  && PATH="$PATH:/bin/vendor_perl"
[ -d "/snap/bin" ]         && PATH="$PATH:/snap/bin"
                              PATH="$PATH:$HOME/.npm-packages/bin"
                              PATH="$PATH:$HOME/.cargo/bin"
                              PATH="$PATH:$HOME/.gem/ruby/bin"
                              PATH="$PATH:$HOME/configs/scripts"
                              PATH="$PATH:$HOME/workspace/configs/scripts"
                              PATH="$PATH:/usr/local/bin:/usr/bin:/bin:/sbin"
                              PATH="$PATH:/usr/games"
                              PATH="$PATH:/usr/local/sbin:/usr/sbin"

export                      MANPATH="$HOME/.man:/usr/local/share/man:/usr/local/man"
[ -d "$HOME/.local" ]   &&  MANPATH="$MANPATH:$HOME/.local/share/man:$HOME/.local/man"
[ -d "$HOME/.cabal" ]   &&  MANPATH="$MANPATH:$HOME/.cabal/share/man"
[ -d "$HOME/.npm-packages" ]   &&  MANPATH="$MANPATH:$HOME/.npm-packages/share/man"
[ -d "/opt/local" ]     &&  MANPATH="$MANPATH:/opt/local/share/man:/opt/local/man"
                            MANPATH="$MANPATH:/usr/share/man:/usr/man"

# export LD_LIBRARY_PATH="$HOME/.local/lib/:$LD_LIBRARY_PATH"

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
export GPG_TTY=$(tty)

#-----------------------------#
# ENVIRONMENT - LOCAL CONFIG  #
#-----------------------------#
debug_shell Profile: Local
# shellcheck source=/dev/null
[ -r "$HOME/.profile.local" ] && . "$HOME/.profile.local"

debug_shell Profile: Done
true # Exit status should be 0.
