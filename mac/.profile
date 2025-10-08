# if [ -e /opt/homebrew ] ; then
#   eval `/opt/homebrew/bin/brew shellenv`
# fi

if [ -e "$HOME/.profile.local" ]; then
    source "$HOME/.profile.local"
fi
