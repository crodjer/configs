
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
export PATH=$HOME/bin:/usr/games:$HOME/.cabal/bin:$HOME/.local/bin:$HOME/.local/sbin:$HOME/workspace/src/golang/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/local/games

# vim bindings
bindkey -e

bindkey '^R' history-incremental-search-backward

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

alias pi='sudo pacman -S'
alias pud='sudo pacman -Sy'
alias pug='sudo pacman -Syu'
alias pugf='pug'
alias pse='pacman -Ss'
alias pr='sudo pacman -R'
# alias pi='sudo aptitude install'
# alias pif='sudo apt-metalink install'
# alias pr='sudo aptitude remove'
# alias pp='sudo aptitude purge'
# alias pud='sudo aptitude update'
# alias pug='sudo aptitude upgrade'
# alias pugf='sudo aptitude dist-upgrade'
# alias pugff='sudo apt-metalink dist-upgrade'
# alias pse='aptitude search'
# alias psh='aptitude show'

alias halt='sudo shutdown -h now'
alias reboot='sudo reboot'
#Save session to disk and bind caps as escape on resume
# alias s2disk='dbus-send --system --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Hibernate && displays'
# alias s2ram='dbus-send --system --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Suspend && displays'
alias s2disk='sudo pm-hibernate && displays'
alias s2ram='sudo pm-suspend && displays'
alias s2both='sudo s2both && displays'
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

alias node='env NODE_NO_READLINE=1 rlwrap node'

alias mute='amixer set Master off'
alias unmute='amixer set Master on'

# alias accounts='e ~
alias acc='e $LEDGER_FILE'
alias bal='ledger balance'

alias mnt='udisksctl mount -b'
alias umnt='udisksctl unmount -b'

#alias e='vim --servername default --remote-silent'
alias e='emacsclient -n'
alias ec='emacsclient -c'
alias et='emacsclient -ct'
alias ef='emacsclient -n -c'
alias sec='sudo emacsclient -c'
alias vnew='vim --servername default'

alias -g ack='perl /bin/vendor_perl/ack'
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

function serve(){
    python -m SimpleHTTPServer ${1:-8000}
}

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
alias rez='exec zsh'

alias pqiv='pqiv -iwP left,top'
alias entertain='mplayer "$(find "." -type f -regextype posix-egrep -regex ".*\.(avi|mkv|flv|mpg|mpeg|mp4|wmv|3gp|mov|divx)" | shuf -n1)"'
alias rand='tr -c "[:digit:]" " " < /dev/urandom | dd cbs=$COLUMNS conv=unblock | GREP_COLOR="1;32" grep --color "[^ ]"'

alias h='history'
alias clojurex='java -Xmx64m -cp /usr/share/java/clojure-1.4.0.jar:.'
alias kvm='qemu-system-x86_64 -enable-kvm'

### Exports
export EDITOR='emacsclient -ct'
export GOROOT=$HOME/workspace/src/golang
export JAVA_HOME=/usr/lib/jvm/default-java
export PYTHONSTARTUP=$HOME/.pythonrc
export _JAVA_AWT_WM_NONREPARENTING=1
export LEDGER_FILE=$HOME/documents/accounts/personal.dat

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

source `which virtualenvwrapper.sh`

batteryGeneric() {
    TPACPI_BAT=1

    if [[ ! -z "$2" ]]; then
        sudo tpacpi-bat -s $1 $TPACPI_BAT $2
    fi

    sudo tpacpi-bat -v -g $1 $TPACPI_BAT
    PERCENT=$(echo "$(sudo tpacpi-bat -g $1 $TPACPI_BAT  | cut -d ' ' -f 1) - 128" | bc)
    if [[ $PERCENT = 0 ]]; then
        echo "Threshold set to default"
    else
        echo "Threshold set on $PERCENT%"
    fi
}

thinkPadBatteryStartThreshold() {
    batteryGeneric "ST" $1
}
alias tpBatST='thinkPadBatteryStartThreshold'

thinkPadBatteryStopThreshold() {
    batteryGeneric "SP" $1
}
alias tpBatSP='thinkPadBatteryStopThreshold'

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

zlemma(){
    eset zlemma
    cd ~/workspace/zlemma/zlemma
    workon zlemma
    export DJANGO_SETTINGS_MODULE=settings
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

bs() {
    case $1 in
        dump)
            echo "Fetching a ${2:-wtf2} dump"
            ssh wtf 'cd /tmp; mysqldump -u staging -pc0stac0ff33 ${2:-wtf2} > wtf-dump-rohan.sql; gzip wtf-dump-rohan.sql'
            scp wtf:/tmp/wtf-dump-rohan.sql.gz .
            ssh wtf 'rm /tmp/wtf-dump-rohan.sql.gz'
            gunzip wtf-dump-rohan.sql.gz
            mysql -uroot cbtwtf < wtf-dump-rohan.sql
            rm wtf-dump-rohan.sql
            ;;
        cd)
            cd ~/workspace/bs/railsApp
            ;;
        c)
            bs cd
            rails c
            ;;
        s)
            bs cd
            rails s
            ;;
        git)
            git config user.email rohan@browserstack.com
            git config remote.origin.url `git config remote.origin.url  | sed s/github.com/github-bs/`
            ;;
        *)
            rvm use ree
            bs cd
            ;;
    esac
}

# Rooter; https://github.com/yeban/rooter.sh
# . $HOME/workspace/scripts/rooter.sh/rooter.sh

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
    # case $first in
    #     alpine)
    #     #irssi|twitter|ncmpcpp|mutt)
    #         export LD_PRELOAD=/usr/lib/libtsocks.so
    #         ;;
    # esac

    # automatically use proxychains for some programs
    # case $first in
    #     alpine)
    #         export LD_PRELOAD=/usr/lib/libproxychains.so.3
    #         ;;
    # esac

}

precmd () {
    # reset LD_PRELOAD khat might have been set in preexec()
    export LD_PRELOAD=''
}

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
[[ -s "/etc/profile.d/autojump.zsh" ]] && source "/etc/profile.d/autojump.zsh"
export XAUTHORITY=/home/rohan/.Xauthority

### Added by the Heroku Toolbelt
export PATH="$PATH:/usr/local/heroku/bin"
export RUBY_HEAP_MIN_SLOTS=800000
export RUBY_HEAP_FREE_MIN=100000
export RUBY_HEAP_SLOTS_INCREMENT=300000
export RUBY_HEAP_SLOTS_GROWTH_FACTOR=1
export RUBY_GC_MALLOC_LIMIT=79000000

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
