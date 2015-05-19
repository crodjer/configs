#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#-------------------------#
# Bash configuration
#-------------------------#
shopt -s histappend autocd globstar
export HISTTIMEFORMAT="%F %T "
export HISTCONTROL=ignoredups:ignorespace:erasedups
export HISTSIZE=
export HISTFILESIZE=
export HISTIGNORE="ls:la:l:ll:lla:[bf]g:clear:exit"
PS1='[\u@\h \W]\$ '
exists() {
    test -x "$(command -v "$1")"
}

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

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

exists pacman && {
  alias p='sudo pacman'
  alias pi='p -S'
  alias pud='p -Sy'
  alias pug='p -Syu'
  alias pr='p -R'
  alias pugf='yaourt -Syu --aur --noconfirm'
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

alias node='env NODE_NO_READLINE=1 rlwrap node'
alias npm='PREFIX=/home/rohan/.local npm'

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
alias ed='e /home/rohan/workspace/trash/dumppad.md'

alias ls='ls --color=auto'
alias l='ls'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -al'
# alias -g G='| grep'
# alias -g L='| less'

alias reb='exec bash'
# alias ssh='ssh'
# alias cssh='cssh'
alias screen='TERM=screen-256color screen -U'
alias t='task'

# Git aliases
source /usr/share/bash-completion/completions/git
git_branch () {
    git branch | grep "*" | cut -d " " -f 2
}
alias g="git"
alias ga="git add"
__git_complete ga _git_add
alias gc="git commit -v"
__git_complete gc _git_commit
alias gca="git commit -va"
__git_complete gca _git_commit
alias gst="git status"
__git_complete gst _git_status
alias gco="git checkout"
__git_complete gco _git_checkout
alias gl="git pull"
__git_complete gl _git_pull
alias gp="git push"
__git_complete gp _git_push
alias gup="git fetch"
__git_complete gup _git_fetch
alias glg='git log --stat'
__git_complete glg _git_log
alias gcm='git checkout master'
alias gcd='git checkout develop'
alias ggpush='gp origin $(git_branch)'
alias ggprnp='gl --rebase origin $(git_branch) && ggpush'
alias gglr='gl --rebase origin $(git_branch)'
alias sb-ghc="cabal exec ghc"
alias sb-ghci="cabal exec ghci"
alias sb-runhaskell="cabal exec runhaskell"
alias sb-ghc-pkg="cabal exec ghc-pkg"

function ghc-pkg-reset() {
    read -p 'Erasing all your user ghc and cabal packages. Are you sure (y/n)? ' ans
    test "$ans" == "y" && {
        echo 'Erasing directories under ~/.ghc'
        rm -rf `find ~/.ghc -maxdepth 1 -type d`

        echo 'Erasing ~/.cabal/lib'
        rm -rf ~/.cabal/lib
        # echo 'erasing ~/.cabal/packages'; rm -rf ~/.cabal/packages; \
        # echo 'erasing ~/.cabal/share'; rm -rf ~/.cabal/share; \
    }
}

# MPC aliases
alias m="mpc"
alias mst="mpc -f '%artist% - %title%\n%album%\n%file%' status"
alias mtog="mpc toggle"
#Search song from playlist and also get the song #
alias sose="mpc playlist -f '%title% \[%album%\]' | grep -in"

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

function b() {
    xbacklight -set $(($1 + 1))
}

batteryGeneric() {
    TPACPI_BAT=1

    if [[ ! -z "$2" ]]; then
        sudo tpacpi-bat -s $1 $TPACPI_BAT $2
    fi

    sudo tpacpi-bat -v -g $1 $TPACPI_BAT
    PERCENT=$(echo "$(sudo tpacpi-bat -g $1 $TPACPI_BAT  | cut -d ' ' -f 1)" | bc)
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

alias entertain='mplayer "$(find "." -type f -regextype posix-egrep -regex ".*\.(avi|mkv|flv|mpg|mpeg|mp4|wmv|3gp|mov|divx)" | shuf -n1)"'
alias rand='tr -c "[:digit:]" " " < /dev/urandom | dd cbs=$COLUMNS conv=unblock | GREP_COLOR="1;32" grep --color "[^ ]"'

alias h='history'
alias hn='history -n'
alias cv='command -v'

alias dbx='dropbox.py'

function serve() {
    python3 -m http.server ${1:-8000}
}

#-------------------------#
# Completion
#-------------------------#
re_comp() {
    [[ -e /usr/share/bash-completion/completions/$1 ]] && {
        source /usr/share/bash-completion/completions/$1
        complete -F _$1 $2
    }
}
re_comp git g
re_comp mpc m
re_comp pacman p
re_comp task t
re_comp command cv
complete -F _t tw

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
        _PVENV=" $blue($(basename $VIRTUAL_ENV))"
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
    history -a; history -n

    local title=$1
    local excluded_commands=(
        source ls clear echo exec cat printf cd
        $(echo $PROMPT_COMMAND | sed 's/;//')
    )
    local exclude_re=$(printf '%s\n' "${excluded_commands[@]}" | paste -sd '|')
    local short_title=$(echo "$title" \
        | sed -r "s/^\s*(\w+=\w+\s+)*//g" \
        | sed -r "s/^\s*sudo\s+//g" \
        | cut -d " " -f 1)

    if [[ $short_title =~ $exclude_re ]]; then
        # Matches the excluded title. We don't want to set as shell title.
        unset title
    fi

    if [ -z "$title" ]; then
        # No title available, build it based on user, host and pwd
        _pwd=$PWD
        [[ "$_pwd" =~ ^"$HOME"(/|$) ]] && _pwd="~${_pwd#$HOME}"
        title="$USER@$(hostname):$_pwd"
        short_title=$(basename "$PWD")
    fi

    if [[ "$EMACS" ]]; then
        return;
    fi

    if [[ "$STY" ]]; then
        # I am in a screen session, set the short title
        printf "\033k$short_title\033\\" >&2
    fi

    printf "\e]0;%s\007" "$title" >&2
}

# Inspired by http://superuser.com/a/175802/57412
preexec_invoke_exec () {
    [ -n "$COMP_LINE" ] && return  # do nothing if completing

    if [[ $PROMPT_COMMAND =~ .*$BASH_COMMAND.* ]]; then
        # The current command is one the prompt command i.e. a prompt
        # execution.
        preexec "$BASH_COMMAND"
        _prompt_wait=true
        return
    fi

    if [ -z "$_prompt_wait" ]; then
        # Probably a non manual execution, don't care about those.
        return
    fi

    preexec "$BASH_COMMAND"
    unset _prompt_wait
}
trap 'preexec_invoke_exec' DEBUG

exists virtualenvwrapper.sh && source `which virtualenvwrapper.sh`
[[ -s "/etc/profile.d/autojump.bash" ]] && source "/etc/profile.d/autojump.bash"

#-------------------------#
# Functions
#-------------------------#
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
    cd ~/workspace/src/django && workon django
}

zlemma(){
    zlemma_parent=/home/rohan/workspace/zlemma

    export DJANGO_SETTINGS_MODULE=zdb_common.settings.local
    env='rp'
    project='zlemma'

    case $1 in
        rp)
            env='em'
            project='resume-parser'
            export EM_NAMES_SOURCE=file://$HOME/workspace/zlemma/resume-parser/tmp/rp2_data/experimental-data/
            export RP_STATIC_DATA_PATH=~/Dropbox/rp2_data

            function rp() {
                python -m resume_parser $1 2>&1
            }
            function shd() {
                ./scripts/section_headings.py $1 2>&1
            }
            function rp-emp() {
                rp $1 | grep Employer | less
            }
            function rp-jt() {
                rp $1 | grep 'PositionTitle\|JobTitle' | less
            }
            ;;
        em)
            env='em'
            project='entity-match'
            ;;
        redis*)
            zlemma rp
            cd ../tmp/
            redis-server
            ;;
        re*)
            env='inscoring'
            project='recruiterservice'
            ;;
        sc*)
            env='inscoring'
            project='scoringservice'
            ;;
        ex*)
            env='zlemma'
            project='browserextensions'
            ;;
        *)
            env='rp'
            project='zlemma'
            export DJANGO_SETTINGS_MODULE=settings.local
            ;;
    esac

    deactivate 2> /dev/null
    cd $zlemma_parent/$project
    export PYTHONPATH=$PWD:$PYTHONPATH
    workon $env
}

hr(){
    cd ~/workspace/is/ruby/hackerrank
    alias gpr='gl --rebase && gp'
}

s() {
    find . -iname "*$@*"
}

#Search for text in files
sg() {
    if [ $# -gt 1 ];then
        find . -iname "$1" | xargs grep "$2"
    else
        echo 'Input the file and grep patterns'
    fi
}
