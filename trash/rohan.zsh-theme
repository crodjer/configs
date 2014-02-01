# user, host, full path, and time/date
# on two lines for easier vgrepping
# entry in a nice long thread on the Arch Linux forums: http://bbs.archlinux.org/viewtopic.php?pid=521888#p521888

# Mercurial prompt
# This requires Steve Losh's hg-prompt (http://stevelosh.com/projects/hg-prompt/)
# to be installed
function hg_prompt_info {
    hg prompt "{branch}-{tags|,}{status|modified|unknown}{update}"
}

function vc_info {
    VC_CHAR=''

    if git branch >/dev/null 2>/dev/null
    then
        # This is a git repo
        VC_CHAR='±'
        VC_INFO="$(git_prompt_info) $(git_prompt_status)"
    elif hg root >/dev/null 2>/dev/null
    then
        # This is a mercurial repo
        VC_CHAR="☿"
        VC_INFO=$(hg_prompt_info)
    fi
    if [ -n $VC_CHAR ]
    then
        echo " %{\e[1;32m%}%B$VC_CHAR%b %{\e[0;36m%}$VC_INFO"
    fi
}

function status_code {
    EXIT_STATUS="$?"

    if [[ $EXIT_STATUS != "0" ]]; then
        echo "%b-%{\e[0;34m%}%B[%b%b-%{\e[0;31m%}$EXIT_STATUS%{\e[1;34m%}%B]"
    fi

    unset EXIT_STATUS
}

PROMPT=$'%{\e[0;34m%}%B┌─[%b%{\e[0m%}%{\e[1;32m%}%n%{\e[1;30m%}@%{\e[0m%}%{\e[0;36m%}%m%{\e[0;34m%}%B]%b%{\e[0m%}-%{\e[0;34m%}%B[%b%{\e[0;33m%}'%D{"%I:%M %p"}%b$'%{\e[0;34m%}%B]%b$(status_code)%{\e[0m%}$(vc_info)
%{\e[0;34m%}%B└─%B%(!.#.>)%{\e[0m%}%b '
#PS1=$'%{\e[0;34m%}%B└─%B%(!.#.>)%{\e[0m%}%b '

RPS1=$'%{\e[0;32m%}%B%~%b'
