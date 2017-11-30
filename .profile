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
                              PATH="$PATH:$HOME/.npm-packages/bin"
                              PATH="$PATH:$HOME/.cargo/bin"
                              PATH="$PATH:$HOME/.gem/ruby/bin"
                              PATH="$PATH:$HOME/configs/scripts"
                              PATH="$PATH:$HOME/workspace/configs/scripts"
                              PATH="$PATH:/usr/local/bin:/usr/bin:/bin:/sbin"
                              PATH="$PATH:/usr/local/sbin:/usr/sbin"

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

command -v nvim > /dev/null && EDITOR='nvim' || EDITOR='vi'
export EDITOR

export HOSTALIASES=~/.hosts

if [ "$(uname)" = Darwin ]; then
    export MACOS=true
fi

function make_home () {
    local usage="$0 [-h <host>] [-x] [-c <configs-dir>]"
    usage="$usage\n\th: the remote host"
    usage="$usage\n\tc: the source configs directory (default: ~/configs/)"
    usage="$usage\n\tx: actually run commands (dry run by default)"

    local dry_run="true"
    local configs=(
        .zshrc
        .profile
        .zprofile
        .inputrc
        .bashrc
        .bash_profile
        .gitconfig
        .gitignore
        .config/nvim/init.vim
    )

    local remote
    local host
    local configs_src
    local config

    if [[ -d "$HOME/configs/" ]]; then
        configs_src="$HOME/configs"
    else
        configs_src="$HOME"
    fi

    while getopts "hr:c:x" opt; do
        case $opt in
            h)
                echo "$usage"
                return 0
                ;;
            r)
                remote="true"
                host=$OPTARG
                ;;
            x)
                unset dry_run
                ;;
            c)
                configs_src=$OPTARG
                ;;
            \?)
                echo "$usage" >&2
                return 1
                ;;
        esac
    done

    function run () {
            echo "$ $*"
        if [ -z "$dry_run" ]; then
            eval $*
        fi
    }

    for config in "${configs[@]}"
    do
        local parent_dir="$(dirname "$config")"

        if [ $parent_dir = "." ]; then
            unset parent_dir
        fi

        if [ "$remote" ]; then
            if [ "$parent_dir" ]; then
                run ssh $host mkdir -p "'~/$(dirname $config)'"
            fi
            run scp "$configs_src/$config" $host:~/$config
        else
            if [ "$parent_dir" ]; then
                run mkdir -p "~/$(dirname $config)" &> /dev/null
            fi
            run ln -s "$configs_src/$config" "~/$config"
        fi
    done
}
export make_home

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
export CHESSDIR=$HOME/.chess/
export GEM_HOME=$HOME/.gem/ruby/
export AUTOSSH_POLL=60
export SCIKIT_LEARN_DATA=~/.scikit_learn_data

#-----------------------------#
# ENVIRONMENT - LOCAL CONFIG  #
#-----------------------------#
# shellcheck source=/dev/null
[ -r "$HOME/.profile.local" ] && . "$HOME/.profile.local"
# [ "$BASH_VERSION" -a -z "$POSIXLY_CORRECT" ] && . "$HOME/.bashrc"

true # Exit status should be 0.
