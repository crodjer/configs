#!/bin/sh
#
# POSIX Shell login script.
#
#   Rohan Jain
#       ~crodjer
#
# Borrowed from: http://stuff.lhunath.com/.profile

#-------------------------#
# BASE - PATH             #
#-------------------------#
export                        PATH="$HOME/.bin"
[ -d "$HOME/.local" ]      && PATH="$PATH:$HOME/.local/bin:$HOME/.local/sbin"
[ -d "$HOME/.cabal" ]      && PATH="$PATH:$HOME/.cabal/bin"
[ -d "/opt/local" ]        && PATH="$PATH:/opt/local/bin:/opt/local/sbin"
[ -d "/bin/vendor_perl" ]  && PATH="$PATH:/bin/vendor_perl"
                              PATH="$PATH:/opt/clojurescript/bin"
                              PATH="$PATH:$HOME/.gem/ruby/bin"
                              PATH="$PATH:$HOME/.gem/ruby/2.0.0/bin"
                              PATH="$PATH:$HOME/.gem/ruby/2.2.0/bin"
                              PATH="$PATH:$HOME/workspace/configs/scripts"
                              PATH="$PATH:/usr/local/bin:/usr/bin:/bin:/sbin"
                              PATH="$PATH:/usr/local/sbin:/usr/sbin"
# [ "$(id -u)" -eq "0" ]  &&    PATH="$PATH:/usr/local/sbin/:/usr/sbin"

export                  MANPATH="$HOME/.man:/usr/local/share/man:/usr/local/man"
[ -d "$HOME/.local" ]&& MANPATH="$MANPATH:$HOME/.local/share/man:$HOME/.local/man"
[ -d "$HOME/.cabal" ]&& MANPATH="$MANPATH:$HOME/.cabal/share/man"
[ -d "/opt/local" ]  && MANPATH="$MANPATH:/opt/local/share/man:/opt/local/man"
                        MANPATH="$MANPATH:/usr/share/man:/usr/man"

# export LD_LIBRARY_PATH="$HOME/.local/lib/:$LD_LIBRARY_PATH"

#-------------------------#
# BASE - Environment
#-------------------------#
[ -e /usr/share/zoneinfo/Asia/Kolkata ] && {
    export TZ="/usr/share/zoneinfo/Asia/Kolkata"
}
command -v emacsclient > /dev/null && export EDITOR='emacsclient -ct' || {
    command -v zile && export EDITOR=zile
} || {
    export EDITOR='vi'
}
export HOSTALIASES=~/.hosts

if [ "$(uname)" = Darwin ]; then
    export MACOS=true
fi

#-----------------------------#
# ENVIRONMENT - APPLICATIONS  #
#-----------------------------#
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
# export GOPATH=/home/rohan/workspace/go
# export PATH=$PATH:$GOPATH/bin
export CHESSDIR=$HOME/.chess/
export GEM_HOME=$HOME/.gem/ruby/
export AUTOSSH_POLL=60

#-----------------------------#
# ENVIRONMENT - LOCAL CONFIG  #
#-----------------------------#
# shellcheck source=/dev/null
[ -r "$HOME/.profile.local" ] && . "$HOME/.profile.local"
# [ "$BASH_VERSION" -a -z "$POSIXLY_CORRECT" ] && . "$HOME/.bashrc"

true # Exit status should be 0.
