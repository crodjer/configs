#!/bin/sh
#
# POSIX Shell login script.
#
#   Rohan Jain
#       ~crodjer
#
# Borrowed from: http://stuff.lhunath.com/.profile

#-------------------------#
# BASE - UTILITY          #
#-------------------------#
exists() {
    command -v "$1" &> /dev/null
}

#-------------------------#
# BASE - PATH             #
#-------------------------#
export                        PATH="$HOME/.bin"
[ -d "$HOME/.local" ]      && PATH="$PATH:$HOME/.local/bin:$HOME/.local/sbin"
[ -d "$HOME/.cabal" ]      && PATH="$PATH:$HOME/.cabal/bin"
[ -d "/opt/local" ]        && PATH="$PATH:/opt/local/bin:/opt/local/sbin"
[ -d "/bin/vendor_perl" ]  && PATH="$PATH:/bin/vendor_perl"
                              PATH="$PATH:$HOME/.gem/ruby/2.2.0/bin"
                              PATH="$PATH:$HOME/workspace/configs/scripts/"
                              PATH="$PATH:/usr/local/bin:/usr/bin:/bin"
[ "$(id -u)" -eq "0" ]  &&    PATH="$PATH:/usr/local/sbin/:/usr/sbin"


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
exists emacsclient && export EDITOR='emacsclient -ct' || {
    exists zile && export EDITOR=zile
} || {
    export EDITOR='vi'
}
export HOSTALIASES=~/.hosts

#-------------------------#
# SHELL - CHECK TYPE      #
#-------------------------#

# If not running interactively, ignore following definitions.
case "$-" in *i*) ;; *) return;; esac

#-----------------------------#
# ENVIRONMENT - APPLICATIONS  #
#-----------------------------#
export LESS=" -R "
exists source-highlight-esc.sh && \
    export LESSOPEN="| `which source-highlight-esc.sh` %s 2> /dev/null"
export GREP_COLOR=31
export PAGER=less
export MANPAGER=$PAGER
export NODE_PATH=/home/rohan/.local/lib/node_modules:/usr/lib/node_modules
# export GOPATH=/home/rohan/workspace/go
# export PATH=$PATH:$GOPATH/bin
export CHESSDIR=/home/rohan/.chess/
export GEM_HOME=/home/rohan/.gem/ruby/

#-----------------------------#
# ENVIRONMENT - LOCAL CONFIG  #
#-----------------------------#
[ -r "$HOME/.profile.local" ] && . "$HOME/.profile.local"
# [ "$BASH_VERSION" -a -z "$POSIXLY_CORRECT" ] && . "$HOME/.bashrc"

true # Exit status should be 0.
