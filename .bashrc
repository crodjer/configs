#!/usr/bin/env bash
#
# ~/.bashrc
#

# Depends on my ~/.profile

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

exists () {
    command -v "$1" &> /dev/null
}

if [[ $(uname -a) =~ Darwin ]]; then
    export MACOS=true
fi

#-------------------------#
# Bash configuration
#-------------------------#
shopt -s histappend autocd globstar histreedit histverify

export HISTTIMEFORMAT="%F %T "
export HISTCONTROL=ignorespace:ignoredups:erasedups
export HISTSIZE=
export HISTFILESIZE=
export HISTIGNORE="ls:la:l:ll:lla:lls:sl:gst:glg:g diff:..::...:s:mtog:cd:cd -:clear:reb:exit"
PS1='[\u@\h \W]\$ '

# Enabled blocked forward incrmental search
# http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=383760
stty -ixon

#-------------------------#
# Aliases
#-------------------------#
alias df='df -h'
alias du='du -hs'
alias less='less'
alias free='free -m'
alias netctl='sudo netctl'
alias netctl-auto='sudo netctl-auto'
alias n='sudo netctl'
alias na='sudo netctl-auto'
alias wvdial='sudo wvdial'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

exists pacman && {
  alias p='sudo pacman'
  alias pi='p -S'
  alias pud='p -Sy'
  alias pug='p -Syu'
  alias pr='p -R'
  alias pugf='p -Syu'
  alias pse='pacman -Ss'
  alias psh='pacman -Si'
  alias pshi='pacman -Qi'
  alias pclean='p -Sc'
  alias remove-orphans='p -Rns $(pacman -Qqtd)'
}

exists aptitude && {
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
  alias pclean='sudo aptitude clean'
}

alias halt='sudo shutdown -h now'
alias reboot='sudo reboot'
#Save session to disk and bind caps as escape on resume
# alias s2disk='dbus-send --system --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Hibernate && displays'
# alias s2ram='dbus-send --system --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Suspend && displays'
# alias s2disk='sudo pm-hibernate && display.sh'
# alias s2ram='sudo pm-suspend && display.sh'
# alias s2both='sudo s2both && display.sh'

alias s2disk='sudo systemctl hibernate && display.sh'
alias s2ram='sudo systemctl suspend && display.sh'
alias s2both='sudo systemctl hybrid-sleep && display.sh'

# alias npm='PREFIX=/home/rohan/.local npm'

alias mnt='udisksctl mount -b'
alias umnt='udisksctl unmount -b'
alias ejct='udisksctl power-off -b'


exists emacsclient && {
    alias e='emacsclient -n'
    alias ec='emacsclient -c'
    alias et='emacsclient -ct'
    alias ef='emacsclient -n -c'
}

exists vim && {
    alias vnew='vim --servername default'
}

alias eb='e ~/.bashrc'
alias ez='e ~/.zshrc'
alias ev='e ~/.vimrc'
alias ee='e ~/.emacs'
#A pad to dump arbit data
alias ed='e $HOME/workspace/trash/dumppad.md'

if [[ $MACOS ]]; then
    alias ls='ls -G'
else
    alias ls='ls --color=auto'
fi
alias l='ls'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -al'
# alias -g G='| grep'
# alias -g L='| less'

alias reb='exec bash'
# alias ssh='ssh'
# alias cssh='cssh'
if [[ $MACOS ]]; then
    alias screen='screen -U'
else
    alias screen='screen -U'
fi

alias t='task'
alias tw='task project:Work'
alias tp='task project:Personal'
alias i='task add +in'
tickle () {
    deadline=$1
    shift
    i +tickle wait:$deadline $@
}
alias think='tickle +1d'
alias tick=tickle
alias rnd='task add +rnd +next +@computer +@online'
webpage_title (){
    wget -qO- "$*" | hxselect -s '\n' -c  'title' 2>/dev/null
}
read_and_review (){
    link="$1"
    title=$(webpage_title $link)
    echo $title
    descr="\"Read and review: $title\""
    id=$(task add +next +rnr "$descr" | sed -n 's/Created task \(.*\)./\1/p')
    task "$id" annotate "$link"
}
alias rnr=read_and_review

git_branch () {
    git branch | grep "\*" | cut -d " " -f 2
}

alias g='git'
alias ga='git add'
alias gc='git commit -v'
alias gca='git commit -va'
alias gst='git status'
alias gco='git checkout'
alias gl='git pull'
alias gp='git push'
alias gup="git fetch"
alias glg='git log --stat'
alias gcm='git checkout master'
alias gcd='git checkout develop'
alias ggpush='gp origin $(git_branch)'
alias ggprnp='gl --rebase origin $(git_branch) && ggpush'
alias gglr='gl --rebase origin $(git_branch)'
alias sb-ghc="cabal exec ghc"
alias sb-ghci="cabal exec ghci"
alias sb-runhaskell="cabal exec runhaskell"
alias sb-ghc-pkg="cabal exec ghc-pkg"
alias sx="stack exec"
alias sdcv="sdcv -c"

# Git aliases
if [ -e /usr/local/etc/bash_completion.d/git-completion.bash ]; then
    # shellcheck source=/dev/null
    source /usr/local/etc/bash_completion.d/git-completion.bash
elif [ -e /usr/share/bash-completion/completions/git ]; then
    # shellcheck source=/dev/null
    source /usr/share/bash-completion/completions/git
fi

exists __git_complete && {
    __git_complete ga _git_add
    __git_complete gc _git_commit
    __git_complete gca _git_commit
    __git_complete gst _git_status
    __git_complete gco _git_checkout
    __git_complete gl _git_pull
    __git_complete gp _git_push
    __git_complete gup _git_fetch
    __git_complete glg _git_log
}

function ghc-pkg-reset() {
    read -rp 'Erasing all your user ghc and cabal packages. Are you sure (y/n)? ' ans
    test "$ans" == "y" && {
        echo 'Erasing directories under ~/.ghc'
        rm -rf "$(find ~/.ghc -maxdepth 1 -type d)"

        echo 'Erasing ~/.cabal/lib'
        rm -rf ~/.cabal/lib
        # echo 'erasing ~/.cabal/packages'; rm -rf ~/.cabal/packages; \
        # echo 'erasing ~/.cabal/share'; rm -rf ~/.cabal/share; \
    }
}

# Stack
exists stack && eval "$(stack --bash-completion-script stack)"

# MPC aliases
alias m="mpc"
alias mst="mpc -f '%artist% - %title%\n%album%\n%file%' status"
alias mtog="mpc toggle"
#Search song from playlist and also get the song #
alias sose="mpc playlist -f '%title% \[%album%\]' | grep -in"

# Alters the mpd volume according to the sign and factor of 5
function _alter_mpd_vol(){
    num=$(echo "5*${2:-1}" | bc)
    mpc volume "$1$num"
}

#Increase Vol
function mup(){
    _alter_mpd_vol "+" "$1"
}
#Decrease Vol
function mdw(){
    _alter_mpd_vol "-" "$1"
}


function itunes() {
    osascript -e "tell application \"iTunes\" ${2:-to} $1";
}

function b() {
    xbacklight -set $(($1 + 1))
}

batteryGeneric() {
    TPACPI_BAT=1

    if [[ ! -z "$2" ]]; then
        sudo tpacpi-bat -s "$1" $TPACPI_BAT "$2"
    fi

    sudo tpacpi-bat -v -g "$1" $TPACPI_BAT
    PERCENT=$(sudo tpacpi-bat -g "$1" $TPACPI_BAT  | cut -d ' ' -f 1 | bc)
    if [[ $PERCENT = 0 ]]; then
        echo "Threshold set to default"
    else
        echo "Threshold set on $PERCENT%"
    fi
}

thinkPadBatteryStartThreshold() {
    batteryGeneric "ST" "$1"
}
alias tpBatST='thinkPadBatteryStartThreshold'

thinkPadBatteryStopThreshold() {
    batteryGeneric "SP" "$1"
}
alias tpBatSP='thinkPadBatteryStopThreshold'

alias entertain='mplayer "$(find "." -type f -regextype posix-egrep -regex ".*\.(avi|mkv|flv|mpg|mpeg|mp4|wmv|3gp|mov|divx)" | shuf -n1)"'
alias rand='tr -c "[:digit:]" " " < /dev/urandom | dd cbs=$COLUMNS conv=unblock | GREP_COLOR="1;32" grep --color "[^ ]"'

alias h='history'
alias hn='history -n'
alias cv='command -v'

alias dbx='dropbox.py'
alias gist='gist -c'
alias sgist='gist -p'
alias agist='gist -a'

alias acc='ledger -f ~/.accounts.dat'
alias aria2c='aria2c -j2 --seed-time 0 --max-upload-limit 1'
alias rsync='rsync -vrhP'

function pi-wlan() {
    pi_host=$(arp -na | grep 74:da:38:62:8d:68 \
                      | grep -ohE "\d+.\d+.\d+.\d+")
    echo "Raspberry Pi connected at $pi_host"
    ssh "$pi_host"
}

function serve() {
    python3 -m http.server "${1:-8000}"
}

#-------------------------#
# Completion
#-------------------------#
# Only in mac
if [ -f /usr/local/etc/bash_completion ]; then
    # shellcheck source=/dev/null
    source /usr/local/etc/bash_completion
fi


re_comp() {
    {
        [[ -e /usr/share/bash-completion/completions/$1 ]] && {
            # shellcheck source=/dev/null
            source /usr/share/bash-completion/completions/$1
            complete -F "_$1" "$2"
        }
    } || {
        [[ -e /usr/local/etc/bash_completion.d/$1-completion.bash ]] && {
            # shellcheck source=/dev/null
            source /usr/local/etc/bash_completion.d/$1-completion.bash
            complete -F "_$1" "$2"
        }
    } || {
        [[ -e /usr/local/etc/bash_completion.d/$1 ]] && {
            # shellcheck source=/dev/null
            source /usr/local/etc/bash_completion.d/$1
            complete -F "_$1" "$2"
        }
    } || {
        [[ -e /usr/local/etc/bash_completion.d/$1.sh ]] && {
            # shellcheck source=/dev/null
            source /usr/local/etc/bash_completion.d/$1.sh
            complete -F "_$1" "$2"
        }
    }
}
re_comp git g
re_comp mpc m
re_comp pacman p
re_comp task t
re_comp task tp
re_comp task tw
re_comp command cv
re_comp netctl n
re_comp netctl na
re_comp netctl netctl-auto

#-------------------------#
# Prompt
#-------------------------#

red="\[$(tput setaf 1)\]"
green="\[$(tput setaf 2)\]"
yellow="\[$(tput setaf 3)\]"
blue="\[$(tput setaf 4)\]"
cyan="\[$(tput setaf 6)\]"
bold="\[$(tput bold)\]"
plain="\[$(tput sgr0)\]"

prompt() {
    EXIT_STATUS="$?"

    if git branch >/dev/null 2>/dev/null; then
        # This is a git repo
        VC_CHAR='±'
        VC_REF=$(
            git symbolic-ref --short HEAD 2> /dev/null || {
                git rev-list --max-count=1 1 HEAD 2>/dev/null | cut -c 1-8
            }
        )
        if [[ -z "$(git status --porcelain)" ]]; then
            VC_STATUS=""
        else
            VC_STATUS="*"
        fi
        VC_INFO="$bold$green $VC_CHAR $plain$cyan($VC_REF$VC_STATUS)$plain"
    else
        VC_INFO=""
    fi

    if [[ "$USER" == "root" ]]; then
        _PSYMBOL="#"
    else
        _PSYMBOL=">"
    fi

    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
        _HNCOLOUR=$cyan$bold
    else
        _HNCOLOUR=$cyan
    fi

    if [[ $EXIT_STATUS != "0" ]]; then
        _PUSER_COLOR=$red
    else
        _PUSER_COLOR=$green
    fi

    if [[ -n "$VIRTUAL_ENV" ]];then
        _PVENV=" $blue($(basename "$VIRTUAL_ENV"))"
    else
        _PVENV=""
    fi

    _PUSER="$_PUSER_COLOR$bold\u$green$bold$plain@$_HNCOLOUR\h$plain$blue"
    _PTIME="$plain$yellow\@$blue"
    _PDIR="$plain$green\W$blue"

    _L1="┌─[$_PUSER]-[$_PTIME]$VC_INFO$_PVENV"
    _L2="└─($_PDIR)-$_PSYMBOL"

    PS1="$blue$_L1\n$blue$_L2 $plain"
}

PROMPT_DIRTRIM=2
PROMPT_COMMAND=prompt

preexec () {
    history -a

    if [[ $MACOS ]]; then
        sed=gsed
        paste=gpaste
    else
        sed=sed
        paste=paste
    fi

    if [[ "$EMACS" ]]; then
        return;
    fi
}

# Inspired by http://superuser.com/a/175802/57412
preexec_invoke_exec () {
    [ -n "$COMP_LINE" ] && return  # do nothing if completing

    if [[ $PROMPT_COMMAND == $BASH_COMMAND* ]]; then
        # The current command is one the prompt command i.e. a prompt
        # execution.
        preexec "$BASH_COMMAND"
        return
    fi
}
trap 'preexec_invoke_exec' DEBUG

# shellcheck source=/dev/null
exists virtualenvwrapper_lazy.sh && {
    source "$(which virtualenvwrapper_lazy.sh)"
} || {
    exists virtualenvwrapper.sh &&  source "$(which virtualenvwrapper.sh)"
}

# shellcheck source=/dev/null
[[ -s "/usr/local/etc/profile.d/autojump.sh" ]] && source "/usr/local/etc/profile.d/autojump.sh"

# shellcheck source=/dev/null
[[ -s "/etc/profile.d/autojump.bash" ]] && source "/etc/profile.d/autojump.bash"

export LFS=/mnt/lfs

#-------------------------#
# Functions
#-------------------------#

function s {
    if [ -z $1 ]; then
        echo "A search pattern is required"
        return 1
    fi
    find . -iname "*$**"
}

#Search for text in files
function sg {
    if [ $# -gt 1 ];then
        find . -type f -iname "*$1*" -print0 | xargs grep "$2"
    else
        echo 'Input the search and grep'
    fi
}

function vs {
    export NVIM_LISTEN_ADDRESS=/tmp/$1-vim.sock
    export VI_SERVER=$1-vim
}

# Fire up a new server according to the argument supplied
function vis {
    vim --servername $VI_SERVER
}

# Open up the files in the environment Vim server.
function vic {
    vim --servername $VI_SERVER --remote-silent $*
}

function docker-cleanup {
    if [ $MACOS ]; then
        xargs=gxargs
    else
        xargs=xargs
    fi
    docker ps --no-trunc -aq -f status=exited \
        | $xargs --no-run-if-empty docker rm -v
    docker images -q -f "dangling=true" \
        | $xargs --no-run-if-empty docker rmi
}

function route_to {
    interface=$1
    route="$(ip route \
                 | grep "$interface" \
                 | sed -r 's/\.0\/[[:digit:]]{2,3} /.1 /' \
                 | cut -d ' ' -f -4)"
    if [ -n "$route" ]; then
        sudo ip route del default &> /dev/null
        sudo ip route replace default via "$route"
    else
        echo "Invalid route $interface" >&2
    fi
}

if [ -e "$HOME/.bashrc.local" ]; then
    # shellcheck source=/dev/null
    source "$HOME/.bashrc.local"
fi

# Exit with success
true
