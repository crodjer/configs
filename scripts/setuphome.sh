#!/usr/bin/env bash

# This script is for automating the setting up of fresh home directories
# using the configs present over my github profile.
# "scp"ing my local content everywhere couldn't suffice everytime

# I have tried to make this script usable for everyone with little
# tweeking. Most of it can be configured using environment variables
# and defining new setUp function.

# These are the environment variables:
# The location of destined home, defaults to $HOME
DEST_HOME=${DEST_HOME:-"$HOME"}

# The location of workspace, defaults to $DEST_HOME/workspace
WORKSPACE=${WORKSPACE:-"$DEST_HOME/workspace"}

# The location of config directory, defaults to $WORKSPACE/configs
CONFIG_DIR=${CONFIG_DIR:-"$WORKSPACE/configs"}

# The location of config directory, defaults to $WORKSPACE/configs
SCRIPTS_DIR=${SCRIPTS_DIR:-"$WORKSPACE/scripts"}

# The github handle, defaulting to my name
GITHUB_HANDLE=${GITHUB_HANDLE:-"crodjer"}

# Path which was used to execute this script
THIS_SCRIPT=$0

setExecute(){

    # To save from accidents, by default only dry runs are alowed
    if [ "$1" ]; then
        EXECUTE=true
        echo "Changes will be executed. Be carefull."
    else
        unset EXECUTE
        echo "Changes will not be executed. Will be just a dry run."
    fi


}

cloneRepo(){
    # Clone a repository
    # Allowes for two levels of submodule updates
    # TODO: Utilize submodule recursive
    if [ -d "$1" ]; then
        echo "Directory for $2: $1 exists"
        echo "Updating $2: $1"
        if [ $EXECUTE ]; then
            cd $1
            git pull
            echo "Updating submodules for $2"
            # Init new submodules if any
            git submodule init
            # Update submodules
            git submodule update
        fi
    else
        echo "Making $2 directory: $1 and cloning $2 repo"
        if [ $EXECUTE ]; then
            mkdir -p $1
            cd $1
            git clone https://github.com/$GITHUB_HANDLE/$2 $1
            echo "Updating submodules for $2"
            git submodule init
            git submodule update
        fi

    fi
}

cloneConfigs(){
    # Fetch scripts from https://github.com/crodjer/configs
    cloneRepo $CONFIG_DIR 'configs'
}

cloneScripts(){
    # Fetch scripts from https://github.com/crodjer/scripts
    cloneRepo $SCRIPTS_DIR 'scripts'
}

reloadSethome(){
    # Was useful for testing this script

    echo "Reloading home setup script"
    source $THIS_SCRIPT
    setExecute $1
}

link(){

    # Link source to destination. Source will be have config directory
    # prepended to argument 1. Destination will have destination home
    # directory prepended to argument 2 or 1(if arg 2 is not supplied)

    SOURCE=$CONFIG_DIR/$1
    DEST="$DEST_HOME/${2:-$1}"

    if [[ "$3" == "_parent" ]]; then
        DEST=$(dirname $DEST)/
    fi

    echo "Linking $SOURCE to $DEST"

    if [ $EXECUTE ]; then
        ln -fs  $SOURCE $DEST
    fi

}

mkHomeConfDir(){
    # Some programms store configs in subdirectories of home directory
    # This functions lets you make those subdirectories
    if [ "$1" ]; then
        NEW_HOME_CONF_DIR="$DEST_HOME/.$1"
        if [ ! -d "$NEW_HOME_CONF_DIR" ]; then
            echo "Making $NEW_HOME_CONF_DIR directory"
            if [ $EXECUTE ]; then
                mkdir -p $NEW_HOME_CONF_DIR
            fi
        else
            echo "Directory $NEW_HOME_CONF_DIR already exists not creating"
        fi
            unset NEW_HOME_CONF_DIR
    fi
}

setUp(){
    # Pass on $1 variable, to set dry/wet run
    setExecute $1
    if [ $EXECUTE ];then
        which git &> /dev/null
        if [ ! $? -eq 0 ];then
            echo "Please install git first"
            exit
        fi
    fi

    # Fetch update configuration files
    cloneConfigs

    # Link configuration files to $HOME/file
    link .asoundrc
    link .backup-excludes.txt
    link .bash_profile
    link .bashrc
    link .gitconfig
    link .goobookrc
    link .hgrc
    link .inputrc
    link .irbrc
    link .mailcap
    link .mpdconf
    link .muttrc
    link .polipo
    link .profile
    link .screenlayout
    link .tmux.conf
    link .screenrc
    link .signature
    link .udisks-glue.conf
    link .xinitrc
    link .xmobarrc
    link .Xresources
    link .xsession
    link .xsessionrc
    link .zshrc
    link .guile
    link oh-my-zsh .oh-my-zsh
    link vim .vim

    # $HOME/.config directroy for programs which store conf here
    mkHomeConfDir config
    link config/awesome .config/awesome _parent
    link config/zathura .config/zathura _parent
    link config/uzbl .config/uzbl _parent
    link config/gtk-3.0 .config/gtk-3.0 _parent
    link .mplayer .mplayer _parent

    # $HOME/.xmonad directroy for xmonad configs
    mkHomeConfDir xmonad
    link xmonad/xmonad.hs .xmonad/xmonad.hs

    mkHomeConfDir emacs.d
    mkHomeConfDir emacs.d/el-get
    mkHomeConfDir emacs.d/local
    link .emacs
    link .emacs.d/el-get/.status.el
    link .emacs.d/el-get-user
    link .emacs.d/elisp

    # $HOME/.ncmpcpp for ncmpcpp client configs
    mkHomeConfDir ncmpcpp
    link ncmpcpp/config .ncmpcpp/config

    mkHomeConfDir irssi
    link .irssi/config.tail
    link .irssi/default.theme
    link .irssi/scripts

    chmod +x ~/.xsession*

    mkHomeConfDir local/bin
    link scripts/display.sh .local/bin/display.sh
    link scripts/lock .local/bin/lock
}

# Execute setUp
# Pass on $1 variable, to set dry/wet run
setUp $1
