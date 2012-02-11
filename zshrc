# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="rohan"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git django history-substring-search )

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=/home/rohan/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games:/opt/src/go/bin:/home/rohan/.cabal/bin

# vim bindings
bindkey -v

#No annoying beeps
unsetopt beep

#remove annoying correction options
unsetopt correct_all
setopt correct

alias df='df -h'
alias du='du -hs'
alias less='less'
alias free='free -m'

alias info='info --vi-keys'

alias pi='sudo aptitude install'
alias pif='sudo apt-metalink install'
alias pr='sudo aptitude remove'
alias pp='sudo aptitude purge'
alias pud='sudo aptitude update'
alias pug='sudo aptitude upgrade'
alias pugf='sudo aptitude dist-upgrade'
alias pugff='sudo apt-metalink dist-upgrade'
alias pse='aptitude search'
alias psh='aptitude show'

alias halt='sudo shutdown -h now'
alias reboot='sudo reboot'
#Save session to disk and bind caps as escape on resume
alias s2disk='sudo s2disk'
alias s2both='sudo s2both'

alias mute='amixer set Master off'
alias unmute='amixer set Master on'

alias mnt='udisks --mount'
alias umnt='udisks --unmount'

#alias e='vi'
alias e='gvim --remote-tab-silent'
alias t='python ~/workspace/src/t/t.py --task-dir ~/ --list .tasks --delete-if-empty'

alias -g ack='ack-grep'
alias -g G='| grep'
alias -g L='| less'

alias p='pstr paste'
alias pc='proxychains'
alias sz='source ~/.zshrc'
alias ez='e ~/.zshrc'
alias ev='e ~/.vimrc'
#A pad to dump arbit data
alias ed='e /home/rohan/workspace/trash/dumppad.md'

#Music player shortcuts
alias m="mpc"
alias mstatus="mpc -f '%artist% - %title%\n%album%' status"
alias mtog="mpc toggle"
#Search song from playlist and also get the song #
alias sose="mpc playlist | grep -in"

# Alters the mpd volume according to the sign and factor of 5
function _alter_mpd_vol(){
    num=$( echo "5*${2:-1}" | bc)
    mpc volume $1$num
}

#Increase Vol
function mup(){
    _alter_mpd_vol "+" $1
}
#Decrease Vol
function mdw(){
    _alter_mpd_vol "-" $1
}

#Launch ec2 account
alias ec2='ssh $EC2'

alias pip='pip $@ --proxy="$http_proxy"'
alias hi='ghci 2>&1 | HsColour'

alias clock='tty-clock -ctC 7'

alias pqiv='pqiv -iwP left,top'
alias entertain='mplayer "$(find "." -type f -regextype posix-egrep -regex ".*\.(avi|mkv|flv|mpg|mpeg|mp4|wmv|3gp|mov|divx)" | shuf -n1)"'
alias rand='tr -c "[:digit:]" " " < /dev/urandom | dd cbs=$COLUMNS conv=unblock | GREP_COLOR="1;32" grep --color "[^ ]"'

### Exports
export EDITOR=vim
export JAVA_HOME=/usr
#export PKG_CONFIG_PATH=/home/yeban/opt/lib/pkgconfig/:${PKG_CONFIG_PATH}
#export GOROOT=:/opt/src/go

export LESS='-r'

export PYTHONSTARTUP=$HOME/.pythonrc
export _JAVA_AWT_WM_NONREPARENTING=1
export http_proxy=http://10.3.100.211:8080/
##export http_proxy=http://144.16.192.213:8080/
export https_proxy=$http_proxy

#unset http_proxy
#unset https_proxy

export no_proxy='localhost'

export PIDGIN_DB=~/workspace/src/trash/pidgin.mtn
export PIDGIN_DIR=~/workspace/src/pidgin/
export EC2=ecc.rohanjain.in

source /usr/local/bin/virtualenvwrapper.sh

couchenv(){
    source $HOME/workspace/src/build-couchdb/build/env.sh
    export COUCH='http://admin:admin@localhost:5984'

    ccurl(){
        curl "-vX ${2:-GET} $COUCH/$1"
    }
}

s() { find . -iname "*$@*" }

#Search for text in files
sg() {
    if [ $# -gt 1 ];then
        find . -iname "$1" | xargs grep "$2"
    else
        echo 'Input the file and grep patterns'
    fi
}

. /usr/share/autojump/autojump.sh
# Rooter; https://github.com/yeban/rooter.sh
. $HOME/.zsh/rooter.sh/rooter.sh

preexec () {
    local command=${(V)1//\%\%\%}
    local first=${command%% *}

    # set terminal's title to the currently executing command
    case $TERM in
        xterm|rxvt|rxvt-unicode|screen)
            command=$(print -Pn "%40>...>$command" | tr -d "\n")
            print -Pn "\e]2;$command\a"
            ;;
    esac

    # automatically use tsocks for some programs
    case $first in
        #alpine)
        ncmpcpp|mutt)
            export LD_PRELOAD=/usr/lib/libtsocks.so
            ;;
    esac

    # automatically use proxychains for some programs
    case $first in
        #alpine)
        twitter)
            export LD_PRELOAD=/usr/lib/libproxychains.so.3
            ;;
    esac


    # Set the window title for screen
    if [[ $TERM == "screen"* ]]; then
        local -a cmd; cmd=(${(z)1}) # the command string
        eval "tab_title=$TAB_TITLE_PREFIX$TAB_TITLE_EXEC"
        eval "tab_hardstatus=$TAB_HARDSTATUS_PREFIX$TAB_HARDSTATUS_EXEC"
        screen_set $tab_title $tab_hardstatus
    fi

}

precmd () {
    # reset LD_PRELOAD khat might have been set in preexec()
    export LD_PRELOAD=''
}
