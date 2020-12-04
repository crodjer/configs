#!/usr/bin/env bash

if [ $# -ge 1 ]; then
        game="$(which $1)"
        openbox="$(which openbox)"
        tmpgame="/tmp/tmpgame.sh"
        DISPLAY=:1.0
        echo -e "${openbox} &\nstalonetray &\n${game}" > ${tmpgame}
        echo "starting ${game}"
        chmod +x $tmpgame
        startx ${tmpgame} -- :1 || exit 1
else
        echo "Not a valid argument"
fi
