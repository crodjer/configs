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
    test -x "$(command -v "$1")"
}

#-------------------------#
# BASE - PATH             #
#-------------------------#
export                       PATH="$HOME/.bin:/usr/local/sbin:/usr/local/bin"
[ -d "$HOME/.local" ]     && PATH="$PATH:$HOME/.local/sbin:$HOME/.local/bin"
[ -d "$HOME/.cabal" ]     && PATH="$PATH:$HOME/.cabal/bin"
[ -d "/opt/local" ]       && PATH="$PATH:/opt/local/sbin:/opt/local/bin"
[ -d "/bin/vendor_perl" ] && PATH="$PATH:/bin/vendor_perl"
                             PATH="$PATH:$HOME/.gem/ruby/2.1.0/bin"
                             PATH="$PATH:/usr/sbin:/usr/bin:/sbin:/bin"

export                  MANPATH="$HOME/.man:/usr/local/share/man:/usr/local/man"
[ -d "$HOME/.local" ]&& MANPATH="$MANPATH:$HOME/.local/share/man:$HOME/.local/man"
[ -d "$HOME/.cabal" ]&& MANPATH="$MANPATH:$HOME/.cabal/share/man"
[ -d "/opt/local" ]  && MANPATH="$MANPATH:/opt/local/share/man:/opt/local/man"
                        MANPATH="$MANPATH:/usr/share/man:/usr/man"


#-------------------------#
# BASE - Environment
#-------------------------#
[[ -e /usr/share/zoneinfo/Asia/Kolkata ]] && {
    export TZ="/usr/share/zoneinfo/Asia/Kolkata"
}
export EDITOR='vi'
exists emacsclient && EDITOR='emacsclient -ct' || {
    exists zile && EDITOR=zile;
}
export HOSTALIASES=~/.hosts

#-------------------------#
# SHELL - CHECK TYPE      #
#-------------------------#
case "$-" in *i*) ;; *) return;; esac

#-----------------------------#
# ENVIRONMENT - APPLICATIONS  #
#-----------------------------#
exists source-highlight-esc.sh && \
    export LESSOPEN="| `which source-highlight-esc.sh` %s"
export LESS='-r'
export GREP_COLOR=31
export PAGER=less
export MANPAGER=$PAGER
export NODE_PATH=/home/rohan/.local/lib/node_modules:/usr/lib/node_modules

#-----------------------------#
# ENVIRONMENT - LOCAL CONFIG  #
#-----------------------------#
[ -r "$HOME/.profile.local" ] && \
    . "$HOME/.profile.local"
[ "$BASH_VERSION" -a -z "$POSIXLY_CORRECT" ] && . "$HOME/.bashrc"
