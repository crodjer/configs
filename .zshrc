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
export PATH=$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games:$HOME/.cabal/bin:$HOME/.local/bin:$HOME/workspace/src/golang/bin

# vim bindings
bindkey -e

bindkey '^R' history-incremental-search-backward

#No annoying beeps
unsetopt beep

#remove annoying correction options
unsetopt correct_all
setopt correct

unalias sl

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
alias s2disk='sudo pm-hibernate && display'
alias s2both='sudo s2both && display'
alias s2ram='sudo pm-suspend && display'
alias xsc='xscreensaver-command'

alias ifup='sudo ifup'
alias ifdown='sudo ifdown'
alias ifconfig='sudo ifconfig'
alias ifscheme='sudo ifscheme'

function ifre(){
    ifdown $1
    ifup $1
}

compdef ifre=ifdown

alias mute='amixer set Master off'
alias unmute='amixer set Master on'

alias mnt='udisks --mount'
alias umnt='udisks --unmount'

#alias e='vim --servername default --remote-silent'
alias e='emacsclient -n'
alias ec='emacsclient -c'
alias ef='emacsclient -n -c'
alias sec='sudo emacsclient -c'
alias vnew='vim --servername default'

alias -g ack='ack-grep'
alias -g G='| grep'
alias -g L='| less'

alias p='pstr paste'
alias pc='proxychains'
alias sz='source ~/.zshrc'
alias ez='e ~/.zshrc'
alias ev='e ~/.vimrc'
alias ee='e ~/.emacs'
#A pad to dump arbit data
alias ed='e /home/rohan/workspace/trash/dumppad.md'

alias ssh='TERM=xterm;ssh'
alias cssh='TERM=xterm;cssh'

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

alias st='/usr/bin/gst'

#Launch ec2 account
alias ec2='ssh $EC2'

#alias pip='pip $@ --proxy="$http_proxy"'
alias hi='ghci 2>&1 | HsColour'

alias clock='tty-clock -ctC 0'

alias pqiv='pqiv -iwP left,top'
alias entertain='mplayer "$(find "." -type f -regextype posix-egrep -regex ".*\.(avi|mkv|flv|mpg|mpeg|mp4|wmv|3gp|mov|divx)" | shuf -n1)"'
alias rand='tr -c "[:digit:]" " " < /dev/urandom | dd cbs=$COLUMNS conv=unblock | GREP_COLOR="1;32" grep --color "[^ ]"'

alias h='history'

### Exports
export EDITOR=vi
export GOROOT=$HOME/workspace/src/golang
export JAVA_HOME=/usr/lib/jvm/default-java
export PYTHONSTARTUP=$HOME/.pythonrc
export _JAVA_AWT_WM_NONREPARENTING=1

export LESS='-r'
#export http_proxy=http://144.16.192.247:8080/
#export http_proxy=http://10.3.100.212:8080/
#export https_proxy=$http_proxy

#unset http_proxy
#unset https_proxy

#export no_proxy='localhost'

export PIDGIN_DB=~/workspace/src/trash/pidgin.mtn
export PIDGIN_DIR=~/workspace/src/pidgin/
export EC2=ecc.rohanjain.in
export NODE_PATH=/home/rohan/.local/lib/node_modules

# EC2
export EC2_HOME=$HOME/.local/opt/ec2-api-tools
export EC2_PRIVATE_KEY=~/.ec2/pk-S6B7OSNB5J45TR4S6Q2KHWW4KPJGGM3L.pem
export EC2_CERT=~/.ec2/cert-S6B7OSNB5J45TR4S6Q2KHWW4KPJGGM3L.pem
export PATH=$PATH:$EC2_HOME/bin

export HR_SERVERS='cssh `ec2-describe-instances -F "tag:Name=webserver" | grep INSTANCE | awk "{print $4}"`'

source /usr/local/bin/virtualenvwrapper.sh

couchenv(){
    source $HOME/workspace/src/build-couchdb/build/env.sh
    export COUCH='http://admin:admin@localhost:5984'

    cguid(){
        echo $(curl -X GET $COUCH/_uuids 2> /dev/null G -o '\w\{32\}')
    }

    ccurl(){
        UUID=$(cguid)
        ABS_PATH=$(echo $1 | sed "s/UUID/$UUID/g")
        METHOD=${2:-GET}
        curl -X  $METHOD $COUCH/$ABS_PATH ${@:3}
    }

    ccurlv(){
        UUID=$(cguid)
        ABS_PATH=$(echo $1 | sed "s/UUID/$UUID/g")
        METHOD=${2:-GET}
        curl -vX  $METHOD $COUCH/$ABS_PATH ${@:3}
    }

}

dj(){
    eset django && cd ~/workspace/src/django && workon django
}

hr(){
    eset hrank && cd ~/workspace/is/ruby/hackerrank
    alias gpr='gl --rebase && gp'
}

vs(){
    export VI_SERVER=$1
    vim --servername $VI_SERVER
}

vo() {
    vim --servername $VI_SERVER
}

es(){
    vim --servername $VI_SERVER --remote-silent $*
}

compdef _vim es

eset(){
    export VI_SERVER=$1
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
. $HOME/workspace/scripts/rooter.sh/rooter.sh

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
        alpine)
        #irssi|twitter|ncmpcpp|mutt)
            export LD_PRELOAD=/usr/lib/libtsocks.so
            ;;
    esac

    # automatically use proxychains for some programs
    case $first in
        alpine)
            export LD_PRELOAD=/usr/lib/libproxychains.so.3
            ;;
    esac

}

precmd () {
    # reset LD_PRELOAD khat might have been set in preexec()
    export LD_PRELOAD=''
}

[[ -s "/home/rohan/.rvm/scripts/rvm" ]] && source "/home/rohan/.rvm/scripts/rvm"
export XAUTHORITY=/home/rohan/.Xauthority

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
