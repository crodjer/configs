if [[ -e /opt/homebrew ]] ; then
  # eval $(/opt/homebrew/bin/brew shellenv)
  export PATH=$HOME/.local/bin:$PATH:/opt/homebrew/bin
fi

if [ -e "$HOME/.profile.local" ]; then
    source "$HOME/.profile.local"
fi
