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
[ -d "/opt/local" ]       && PATH="$PATH:/opt/local/sbin:/opt/local/bin"
[ -d "/bin/vendor_perl" ] && PATH="$PATH:/bin/vendor_perl"
                             PATH="$PATH:$HOME/.cabal/bin"
                             PATH="$PATH:/usr/sbin:/usr/bin:/sbin:/bin"

export                  MANPATH="$HOME/.man:/usr/local/share/man:/usr/local/man"
[ -d "$HOME/.local" ]&& MANPATH="$PATH:$HOME/.local/share/man:$HOME/.local/man"
[ -d "/opt/local" ]  && MANPATH="$MANPATH:/opt/local/share/man:/opt/local/man"
                        MANPATH="$MANPATH:/usr/share/man:/usr/man"

#-------------------------#
# BASE - APPLICATIONS     #
#-------------------------#
export EDITOR='vi'
exists emacsclient && EDITOR='emacsclient -ct' || {
    exists zile && EDITOR=zile;
}

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

#-----------------------------#
# ENVIRONMENT - LOCAL CONFIG  #
#-----------------------------#
[ -r "$HOME/.profile.local" ] && \
    . "$HOME/.profile.local"
[ "$BASH_VERSION" -a -z "$POSIXLY_CORRECT" ] && . "$HOME/.bashrc"
