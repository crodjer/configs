#!/usr/bin/env bash

PROGRAM_NAME=$(basename $0)
SANDBOX_DIR=$(echo ${SANDBOX_DIR:-"$HOME/.cabal.sandboxes"} | sed 's/\/$//g')
DEFAULT_BINS=(
    "$HOME/bin"
    "$HOME/.bin"
    "$HOME/.local/bin"
    "$HOME/.cabal/bin"
)

for bin in "${DEFAULT_BINS[@]}"; do
    if [[ ":$PATH:" == *":$bin:"* ]]; then
        AVAILABLE_BIND_DIR=$bin
        break;
    fi
done

BIN_DIR=${BIN_DIR:-$AVAILABLE_BIND_DIR}

_HELP=`cat <<EOF
Usage: $PROGRAM_NAME [OPTIONS] <SANDBOX_NAME>

OPTIONS
-------

 -h          Print this help
 -v          Verbose output
 -r          Re-initialize the sandbox
 -b          Link executables to the bin directory
 -p          Install this package in the sandbox
 -l          Create a sandbox config in current directory
EOF
`

show_help () {
    if [[ "$1" == "stdin" ]]; then
        printf "$_HELP\n"
    else
        printf "$_HELP\n" >&2
    fi
}

error () {
    printf "$@\n"
}

success () {
    printf "$@\n"
}

while getopts :hbvrlp: opt; do
    case $opt in
        v)
            verbose=1
            ;;
        h)
            show_help stdin
            exit 0
            ;;
        l)
            link_pwd=1
            ;;
        r)
            remove=1
            ;;
        b)
            link_binaries=1
            ;;
        p)
            inital_package=$OPTARG
            ;;
        \?)
            error "Invalid option: -$OPTARG" >&2
            show_help
            exit 1
    esac
done

shift $(($OPTIND-1))
sandbox_name="$1"

if [[ ! "$sandbox_name" ]]; then
    show_help
    exit 1
fi

sandbox="$SANDBOX_DIR/$sandbox_name"
cabal_sandbox="$sandbox/.cabal-sandbox"

if [[ -e $sandbox ]]; then
    initialized=1
else
    mkdir -p $sandbox
fi

trigger_dir=$PWD
cd $sandbox

if [[ "$initialized" && "$remove" ]]; then
    cabal sandbox delete 2> /dev/null
fi

if [[ ! -e "$cabal_sandbox" ]]; then
    cabal sandbox init
fi

if [[ "$inital_package" ]]; then
    cabal install "$inital_package"
fi

if [[ "$link_binaries" && -e "$cabal_sandbox/bin/" ]]; then
    ln -s $cabal_sandbox/bin/* $BIN_DIR/
fi

if [[ "$link_pwd" ]]; then
    cd $trigger_dir
    cabal sandbox init --sandbox=$cabal_sandbox
fi
